# Parsec Cloud (https://parsec.cloud) Copyright (c) AGPLv3 2016-2021 Scille SAS

import pytest
import os
import re
import sys
import attr
import json
import ssl
import trustme
import socket
import contextlib
import pendulum
import math
from unittest.mock import patch
import logging
import structlog
import trio
from trio.testing import MockClock
import trio_asyncio
from contextlib import contextmanager, asynccontextmanager
import hypothesis
from pathlib import Path
import sqlite3
import tempfile

from parsec.monitoring import TaskMonitoringInstrument
from parsec.core import CoreConfig
from parsec.core.types import BackendAddr
from parsec.core.core_events import CoreEvent
from parsec.core.logged_core import logged_core_factory
from parsec.core.backend_connection import BackendConnStatus
from parsec.core.mountpoint.manager import get_mountpoint_runner
from parsec.core.fs.storage import LocalDatabase
from parsec.backend import backend_app_factory
from parsec.backend.config import (
    BackendConfig,
    MockedEmailConfig,
    MockedBlockStoreConfig,
    PostgreSQLBlockStoreConfig,
    RAID0BlockStoreConfig,
    RAID1BlockStoreConfig,
    RAID5BlockStoreConfig,
)


# TODO: needed ?
pytest.register_assert_rewrite("tests.event_bus_spy")

from tests.common import (
    freeze_time,
    addr_with_device_subdomain,
    real_clock_timeout,
    get_side_effects_timeout,
)
from tests.postgresql import (
    get_postgresql_url,
    bootstrap_postgresql_testbed,
    reset_postgresql_testbed,
    asyncio_reset_postgresql_testbed,
)
from tests.open_tcp_stream_mock_wrapper import OpenTCPStreamMockWrapper, offline
from tests.event_bus_spy import SpiedEventBus
from tests.fixtures import *  # noqa


from tests.oracles import oracle_fs_factory, oracle_fs_with_sync_factory  # noqa: Republishing


def pytest_addoption(parser):
    parser.addoption("--side-effects-timeout", default=get_side_effects_timeout(), type=float)
    parser.addoption("--hypothesis-max-examples", default=100, type=int)
    parser.addoption("--hypothesis-derandomize", action="store_true")
    parser.addoption(
        "--postgresql",
        action="store_true",
        help=(
            "Use PostgreSQL backend instead of default memory mock "
            "(use `PG_URL` env var to customize the database to use)"
        ),
    )
    parser.addoption("--runslow", action="store_true", help="Don't skip slow tests")
    parser.addoption("--runmountpoint", action="store_true", help="Don't skip FUSE/WinFSP tests")
    parser.addoption("--rungui", action="store_true", help="Don't skip GUI tests")
    parser.addoption("--rundiskfull", action="store_true", help="Don't skip the disk full tests")
    parser.addoption(
        "--realcrypto", action="store_true", help="Don't mock crypto operation to save time"
    )
    parser.addoption("--runrust", action="store_true", help="Don't skip rust tests")
    parser.addoption(
        "--run-postgresql-cluster",
        action="store_true",
        help=(
            "Instead of running the tests, only start a PostgreSQL cluster "
            "that could be use for other tests (through `PG_URL` env var) "
            "to avoid having to create a new cluster each time."
        ),
    )


def is_xdist_master(config):
    return config.getoption("dist") != "no" and not os.environ.get("PYTEST_XDIST_WORKER")


@pytest.fixture(scope="session", autouse=True)
def mock_timezone_utc(request):
    # Mock and non-UTC timezones are a really bad mix, so keep things simple
    with pendulum.test_local_timezone(pendulum.timezone("utc")):
        yield


def pytest_configure(config):
    # Configure structlog to redirect everything in logging
    structlog.configure(
        logger_factory=structlog.stdlib.LoggerFactory(),
        processors=[
            structlog.processors.StackInfoRenderer(),
            structlog.processors.format_exc_info,
            structlog.processors.TimeStamper(fmt="%Y-%m-%d %H:%M:%S"),
            structlog.stdlib.add_logger_name,
            structlog.stdlib.add_log_level,
            structlog.dev.ConsoleRenderer(),
        ],
    )
    # Lock configuration
    structlog.configure = lambda *args, **kwargs: None
    # Add helper to caplog
    patch_caplog()
    if config.getoption("--run-postgresql-cluster"):
        pgurl = bootstrap_postgresql_testbed()
        capturemanager = config.pluginmanager.getplugin("capturemanager")
        if capturemanager:
            capturemanager.suspend(in_=True)
        print(f"usage: PG_URL={pgurl} py.test --postgresql tests")
        input("Press enter when you're done with...")
        pytest.exit("bye")
    elif config.getoption("--postgresql") and not is_xdist_master(config):
        bootstrap_postgresql_testbed()
    # Configure custom side effets timeout
    if config.getoption("--side-effects-timeout"):
        import tests.common

        tests.common._SIDE_EFFECTS_TIMEOUT = float(config.getoption("--side-effects-timeout"))


def patch_caplog():
    from _pytest.logging import LogCaptureFixture

    def _remove_colors(msg):
        return re.sub(r"\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]", "", str(msg))

    def _find(self, log):
        __tracebackhide__ = True
        matches_msgs = []
        matches_records = []
        for record in self.records:
            monochrome_msg = _remove_colors(record.msg)
            if log in monochrome_msg:
                matches_msgs.append(monochrome_msg)
                matches_records.append(record)
        return matches_msgs, matches_records

    def _register_asserted_records(self, *records):
        try:
            asserted_records = self.asserted_records
        except AttributeError:
            asserted_records = set()
            setattr(self, "asserted_records", asserted_records)
        asserted_records.update(records)

    def _assert_occured(self, log):
        __tracebackhide__ = True
        matches_msgs, matches_records = _find(self, log)
        assert matches_msgs
        _register_asserted_records(self, *matches_records)
        return matches_msgs

    def _assert_occured_once(self, log):
        __tracebackhide__ = True
        matches_msgs, matches_records = _find(self, log)
        assert len(matches_msgs) == 1
        _register_asserted_records(self, matches_records[0])
        return matches_msgs[0]

    def _assert_not_occured(self, log):
        __tracebackhide__ = True
        matches_msgs, matches_records = _find(self, log)
        assert not matches_msgs

    LogCaptureFixture.assert_occured = _assert_occured
    LogCaptureFixture.assert_occured_once = _assert_occured_once
    LogCaptureFixture.assert_not_occured = _assert_not_occured


@pytest.fixture(autouse=True)
def no_logs_gte_error(caplog):
    yield
    # The test should use `caplog.assert_occured_once` to indicate a log was expected,
    # otherwise we consider error logs as *actual* errors.
    asserted_records = getattr(caplog, "asserted_records", set())
    errors = [
        record
        for record in caplog.get_records("call")
        if record.levelno >= logging.ERROR and record not in asserted_records
    ]
    assert not errors


@pytest.fixture(scope="session")
def hypothesis_settings(request):
    return hypothesis.settings(
        max_examples=request.config.getoption("--hypothesis-max-examples"),
        derandomize=request.config.getoption("--hypothesis-derandomize"),
        deadline=None,
    )


def pytest_runtest_setup(item):
    if item.get_closest_marker("slow") and not item.config.getoption("--runslow"):
        pytest.skip("need --runslow option to run")
    if item.get_closest_marker("win32") and sys.platform != "win32":
        pytest.skip("test specific to win32")
    if item.get_closest_marker("linux") and sys.platform != "linux":
        pytest.skip("test specific to linux")
    if item.get_closest_marker("mountpoint"):
        if not item.config.getoption("--runmountpoint"):
            pytest.skip("need --runmountpoint option to run")
        elif not get_mountpoint_runner():
            pytest.skip("FUSE/WinFSP not available")
    if item.get_closest_marker("diskfull"):
        if not item.config.getoption("--rundiskfull"):
            pytest.skip("need --rundiskfull option to run")
    if item.get_closest_marker("gui"):
        if not item.config.getoption("--rungui"):
            pytest.skip("need --rungui option to run")
    if item.get_closest_marker("postgresql"):
        if not item.config.getoption("--postgresql"):
            pytest.skip("need --postgresql option to run")
    if item.get_closest_marker("rust") and not item.config.getoption("--runrust"):
        pytest.skip("need --runrust option to run")


def pytest_collection_modifyitems(config, items):
    for item in items:
        if "trio" in item.keywords:
            item.fixturenames.append("task_monitoring")
        if "gui" in item.keywords and "trio" in item.keywords:
            import qtrio

            item.add_marker(pytest.mark.trio(run=qtrio.run))


@pytest.fixture
async def task_monitoring():
    trio.lowlevel.add_instrument(TaskMonitoringInstrument())


@pytest.fixture(autouse=True, scope="session", name="unmock_crypto")
def mock_crypto(request):
    # Crypto is CPU hungry
    if request.config.getoption("--realcrypto"):

        @contextmanager
        def unmock():
            yield

        yield unmock

    else:

        def unsecure_but_fast_argon2i_kdf(size, password, salt, *args, **kwargs):
            data = password + salt
            return data[:size] + b"\x00" * (size - len(data))

        from parsec.crypto import argon2i

        vanilla_kdf = argon2i.kdf

        def unmock():
            return patch("parsec.crypto.argon2i.kdf", new=vanilla_kdf)

        with patch("parsec.crypto.argon2i.kdf", new=unsecure_but_fast_argon2i_kdf):
            yield unmock


@pytest.fixture
def realcrypto(unmock_crypto):
    with unmock_crypto():
        yield


@pytest.fixture
def postgresql_url(request):
    if not request.node.get_closest_marker("postgresql"):
        raise RuntimeError(
            "`postgresql_url` can only be used in tests decorated with `@pytest.mark.postgresql`"
        )
    return get_postgresql_url()


@pytest.fixture
async def asyncio_loop(request):
    # asyncio loop is only needed for triopg
    if not request.config.getoption("--postgresql"):
        yield None

    else:
        # When a ^C happens, trio send a Cancelled exception to each running
        # coroutine. We must protect this one to avoid deadlock if it is cancelled
        # before another coroutine that uses trio-asyncio.
        with trio.CancelScope(shield=True):
            async with trio_asyncio.open_loop() as loop:
                yield loop


@pytest.fixture
def mock_clock():
    # Prevent from using pytest_trio's `mock_clock` fixture.
    raise RuntimeError("Use `frozen_clock` fixture instead !!!")


@pytest.fixture
def autojump_clock():
    # Prevent from using pytest_trio's `autojump` fixture.
    raise RuntimeError("Use `frozen_clock` fixture instead !!!")


@pytest.fixture
def frozen_clock():
    # Mocked clock is a slippy slope: we want time to go faster (or even to
    # jump to arbitrary point in time !) on some part of our application while
    # some other parts should keep using the real time.
    # For instance we want to make sure some part of a test doesn't take more than
    # x seconds in real life (typically to detect deadlock), but this test might
    # be about a ping occurring every 30s so we want to simulate this wait.
    #
    # The simple solution is to use `MockClock.rate` to make time go faster,
    # but it's bad idea given we end up with two antagonistic goals:
    # - rate should be as high as possible so that ping wait goes as fast as possible
    # - the highest rate is, the smallest real time window we have when checking for
    #   deadlock, this is especially an issue given developer machine is a behemoth
    #   while CI run on potatoes (especially on MacOS) shared with other builds...
    #
    # So the solution we choose here is to separate the two times:
    # - Parsec codebase uses the trio clock and `trio.fail_after/move_on_after`
    # - Test codebase can use `trio.fail_after/move_on_after` as long as the test
    #   doesn't use a mock clock
    # - In case of mock clock, test codebase must use `real_clock_timeout` that
    #   relies on monotonic clock and hence is totally isolated from trio's clock.
    #
    # On top of that we must be careful about the configuration of the mock clock !
    # As we said the Parsec codebase (i.e. not the tests) uses the trio clock for
    # timeout handling & sleep (e.g. in the managers), hence:
    # - Using `MockClock.rate` with a high value still lead to the issue dicussed above.
    # - `trio.to_thread.run_sync` doesn't play nice with `MockClock.autojump_threshold = 0`
    #   given trio considers the coroutine waiting for the thread is idle and hence
    #   trigger the clock jump. So a perfectly fine async code may break tests in
    #   an unexpected way if it starts using `trio.to_thread.run_sync`...
    #
    # So the idea of the `frozen_clock` is to only advance when expecially
    # specified in the test (i.e. rate 0 and no autojump_threshold).
    # This way only the test code has control over the application timeout
    # handling, and we have a clean separation with the test timeout (i.e. using
    # `real_clock_timeout` to detect the test endup in a deadlock)
    #
    # The drawback of this approach is manually handling time jump can be cumbersome.
    # For instance the backend connection retry logic:
    # - sleeps for some time
    # - connects to the backend
    # - starts sync&message monitors
    # - message monitor may trigger modifications in the sync monitor
    # - in case of modification, sync monitor is going to sleep for a short time
    #   before doing the sync of the modification
    #
    # So to avoid having to mix `MockClock.jump` and `trio.testing.wait_all_tasks_blocked`
    # in a very complex and fragile way, we introduce the `sleep_with_autojump()`
    # method that is the only place where clock is going to move behind our back, but
    # for only the amount of time we choose, and only in a very explicit manner.
    #
    # Finally, an additional bonus to this approach is we can use breakpoint in the
    # code without worrying about triggering a timeout ;-)

    clock = MockClock(rate=0, autojump_threshold=math.inf)

    clock.real_clock_timeout = real_clock_timeout  # Quick access helper

    async def _sleep_with_autojump(seconds):
        old_rate = clock.rate
        old_autojump_threshold = clock.autojump_threshold
        clock.rate = 0
        clock.autojump_threshold = 0.01
        try:
            await trio.sleep(seconds)
        finally:
            clock.rate = old_rate
            clock.autojump_threshold = old_autojump_threshold

    clock.sleep_with_autojump = _sleep_with_autojump
    yield clock


@pytest.fixture(scope="session")
def unused_tcp_port():
    """Find an unused localhost TCP port from 1024-65535 and return it."""
    with contextlib.closing(socket.socket()) as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


@pytest.fixture(scope="session")
def event_bus_factory():
    return SpiedEventBus


@pytest.fixture
def event_bus(event_bus_factory):
    return event_bus_factory()


@pytest.fixture
def tcp_stream_spy(request, monkeypatch):
    if request.node.get_closest_marker("real_tcp"):
        return None
    else:
        open_tcp_stream_mock_wrapper = OpenTCPStreamMockWrapper()
        monkeypatch.setattr("trio.open_tcp_stream", open_tcp_stream_mock_wrapper)
        monkeypatch.setattr(
            "parsec.core.backend_connection.anonymous.http_request",
            open_tcp_stream_mock_wrapper.http_request,
        )
        return open_tcp_stream_mock_wrapper


@pytest.fixture(scope="session")
def monitor():
    from tests.monitor import Monitor

    return Monitor()


@attr.s
class AppServer:
    entry_point = attr.ib()
    addr = attr.ib()
    connection_factory = attr.ib()

    def offline(self):
        return offline(self.addr)


@pytest.fixture
def server_factory(tcp_stream_spy):
    count = 0

    @asynccontextmanager
    async def _server_factory(entry_point, addr=None):
        nonlocal count
        count += 1

        if not addr:
            addr = BackendAddr(hostname=f"server-{count}.localhost", port=9999, use_ssl=False)

        async def _serve_client(stream):
            if addr.use_ssl:
                ssl_context = ssl.create_default_context()
                stream = trio.SSLStream(
                    stream, ssl_context, server_hostname=addr.hostname, server_side=True
                )
            await entry_point(stream)

        async with trio.open_service_nursery() as nursery:

            def connection_factory(*args, **kwargs):
                client_stream, server_stream = trio.testing.memory_stream_pair()
                nursery.start_soon(_serve_client, server_stream)
                return client_stream

            tcp_stream_spy.push_hook(addr, connection_factory)
            try:
                yield AppServer(entry_point, addr, connection_factory)
                nursery.cancel_scope.cancel()

            finally:
                # It's important to remove the hook just after having cancelled
                # the nursery. Otherwise another coroutine trying to connect would
                # end up with a `RuntimeError('Nursery is closed to new arrivals',)`
                # given `connection_factory` make use of the now-closed nursery.
                tcp_stream_spy.pop_hook(addr)

    return _server_factory


@pytest.fixture()
def backend_addr(tcp_stream_spy, fixtures_customization, monkeypatch):
    # Depending on tcp_stream_spy fixture prevent from doing real connection
    # attempt (which can be long to resolve) when backend is not running
    use_ssl = fixtures_customization.get("backend_over_ssl", False)
    addr = BackendAddr(hostname="example.com", port=9999, use_ssl=use_ssl)
    if use_ssl:
        # TODO: Trustme & Windows doesn't seem to play well
        # (that and Python < 3.7 & Windows bug https://bugs.python.org/issue35941)
        if sys.platform == "win32":
            pytest.skip("Windows and Trustme are not friends :'(")

        # Create a ssl certificate and overload default ssl context generation
        ca = trustme.CA()
        cert = ca.issue_cert("*.example.com", "example.com")
        vanilla_create_default_context = ssl.create_default_context

        def patched_create_default_context(*args, **kwargs):
            ctx = vanilla_create_default_context(*args, **kwargs)
            ca.configure_trust(ctx)
            cert.configure_cert(ctx)  # TODO: only server should load this part ?
            return ctx

        monkeypatch.setattr("ssl.create_default_context", patched_create_default_context)

    return addr


@pytest.fixture(autouse=True)
def persistent_mockup(monkeypatch, fixtures_customization):
    if fixtures_customization.get("real_data_storage", False):
        yield
        return

    @attr.s
    class MockupContext:

        connections = attr.ib(factory=dict)

        def get(self, path):
            if path not in self.connections:
                self.connections[path] = sqlite3.connect(":memory:", check_same_thread=False)
            return self.connections[path]

        def clear(self):
            for connection in self.connections.values():
                try:
                    connection.close()
                except sqlite3.ProgrammingError:
                    # Connections will raise error if they were opened from another
                    # thread. This only occurs for a couple of tests so no big deal.
                    pass
            self.connections.clear()

    mockup_context = MockupContext()
    storage_set = set()

    async def _create_connection(storage):
        storage_set.add(storage)
        storage._conn = mockup_context.get(storage.path)

    async def _close(storage):
        # Idempotent operation
        storage_set.discard(storage)
        storage._conn = None

    async def get_disk_usage(storage):
        return 0

    async def run_in_thread(storage, fn, *args):
        return fn(*args)

    monkeypatch.setattr(LocalDatabase, "run_in_thread", run_in_thread)
    monkeypatch.setattr(LocalDatabase, "_create_connection", _create_connection)
    monkeypatch.setattr(LocalDatabase, "_close", _close)
    monkeypatch.setattr(LocalDatabase, "get_disk_usage", get_disk_usage)

    yield mockup_context
    mockup_context.clear()
    assert not storage_set


@pytest.fixture
def reset_testbed(request, caplog, persistent_mockup):
    async def _reset_testbed(keep_logs=False):
        if request.config.getoption("--postgresql"):
            await trio_asyncio.aio_as_trio(asyncio_reset_postgresql_testbed)
        persistent_mockup.clear()
        if not keep_logs:
            caplog.clear()

    return _reset_testbed


@pytest.fixture()
def backend_store(request):
    if request.config.getoption("--postgresql"):
        reset_postgresql_testbed()
        return get_postgresql_url()

    elif request.node.get_closest_marker("postgresql"):
        pytest.skip("`Test is postgresql-only")

    else:
        return "MOCKED"


@pytest.fixture
def blockstore(request, backend_store, fixtures_customization):
    # TODO: allow to test against swift ?
    if backend_store.startswith("postgresql://"):
        config = PostgreSQLBlockStoreConfig()
    else:
        config = MockedBlockStoreConfig()

    raid = fixtures_customization.get("blockstore_mode", "NO_RAID").upper()
    if raid == "RAID0":
        config = RAID0BlockStoreConfig(blockstores=[config, MockedBlockStoreConfig()])
    elif raid == "RAID1":
        config = RAID1BlockStoreConfig(blockstores=[config, MockedBlockStoreConfig()])
    elif raid == "RAID5":
        config = RAID5BlockStoreConfig(
            blockstores=[config, MockedBlockStoreConfig(), MockedBlockStoreConfig()]
        )
    else:
        assert raid == "NO_RAID"

    return config


@pytest.fixture
async def nursery():
    # A word about the nursery fixture:
    # The whole point of trio is to be able to build a graph of coroutines to
    # simplify teardown. Using a single top level nursery kind of mitigate this
    # given unrelated coroutines will end up there and be closed all together.
    # Worst, among those coroutine it could exists a relationship that will be lost
    # in a more or less subtle way (typically using a factory fixture that use the
    # default nursery behind the scene).
    # Bonus points occur if using trio-asyncio that creates yet another hidden
    # layer of relationship that could end up in cryptic dead lock hardened enough
    # to survive ^C.
    # Finally if your still no convinced, factory fixtures not depending on async
    # fixtures (like nursery is) can be used inside the Hypothesis tests.
    # I know you love Hypothesis. Checkmate. You won't use this fixture ;-)
    raise RuntimeError("Bad kitty ! Bad !!!")


@pytest.fixture
def backend_factory(
    asyncio_loop,
    event_bus_factory,
    backend_data_binder_factory,
    coolorg,
    expiredorg,
    otherorg,
    alice,
    alice2,
    expiredorgalice,
    otheralice,
    adam,
    bob,
    initial_user_manifest_state,
    blockstore,
    backend_store,
    fixtures_customization,
):
    # Given the postgresql driver uses trio-asyncio, any coroutine dealing with
    # the backend should inherit from the one with the asyncio loop context manager.
    # This mean the nursery fixture cannot use the backend object otherwise we
    # can end up in a dead lock if the asyncio loop is torndown before the
    # nursery fixture is done with calling the backend's postgresql stuff.

    @asynccontextmanager
    async def _backend_factory(populated=True, config={}, event_bus=None):
        ssl_context = fixtures_customization.get("backend_over_ssl", False)
        nonlocal backend_store, blockstore
        if fixtures_customization.get("backend_force_mocked"):
            backend_store = "MOCKED"
            assert fixtures_customization.get("blockstore_mode", "NO_RAID") == "NO_RAID"
            blockstore = MockedBlockStoreConfig()

        config = BackendConfig(
            **{
                "administration_token": "s3cr3t",
                "db_min_connections": 1,
                "db_max_connections": 5,
                "debug": False,
                "db_url": backend_store,
                "blockstore_config": blockstore,
                "email_config": None,
                "backend_addr": None,
                "forward_proto_enforce_https": None,
                "ssl_context": ssl_context if ssl_context else False,
                "organization_bootstrap_webhook_url": None,
                "organization_spontaneous_bootstrap": False,
                **config,
            }
        )

        if not event_bus:
            event_bus = event_bus_factory()
        async with backend_app_factory(config, event_bus=event_bus) as backend:
            if populated:
                with freeze_time("2000-01-01"):
                    binder = backend_data_binder_factory(backend)
                    await binder.bind_organization(
                        coolorg,
                        alice,
                        initial_user_manifest=fixtures_customization.get(
                            "alice_initial_remote_user_manifest", "v1"
                        ),
                    )
                    await binder.bind_organization(expiredorg, expiredorgalice)
                    await backend.organization.update(expiredorg.organization_id, is_expired=True)
                    await binder.bind_organization(otherorg, otheralice)
                    await binder.bind_device(alice2, certifier=alice)
                    await binder.bind_device(
                        adam,
                        certifier=alice2,
                        initial_user_manifest=fixtures_customization.get(
                            "adam_initial_remote_user_manifest", "v1"
                        ),
                    )
                    await binder.bind_device(
                        bob,
                        certifier=adam,
                        initial_user_manifest=fixtures_customization.get(
                            "bob_initial_remote_user_manifest", "v1"
                        ),
                    )

            yield backend

    return _backend_factory


@pytest.fixture
async def backend(backend_factory, fixtures_customization, backend_addr):
    populated = not fixtures_customization.get("backend_not_populated", False)
    config = {}
    tmpdir = tempfile.mkdtemp(prefix="tmp-email-folder-")
    config["email_config"] = MockedEmailConfig(sender="Parsec <no-reply@parsec.com>", tmpdir=tmpdir)
    config["backend_addr"] = backend_addr
    if fixtures_customization.get("backend_spontaneous_organization_boostrap", False):
        config["organization_spontaneous_bootstrap"] = True
    if fixtures_customization.get("backend_has_webhook", False):
        # Invalid port, hence we should crash if by mistake we try to reach this url
        config["organization_bootstrap_webhook_url"] = "http://example.com:888888/webhook"
    forward_proto_enforce_https = fixtures_customization.get("backend_forward_proto_enforce_https")
    if forward_proto_enforce_https:
        config["forward_proto_enforce_https"] = forward_proto_enforce_https
    async with backend_factory(populated=populated, config=config) as backend:
        yield backend


@pytest.fixture
def backend_data_binder(backend, backend_data_binder_factory):
    return backend_data_binder_factory(backend)


class LetterBox:
    def __init__(self):
        self._send_email, self._recv_email = trio.open_memory_channel(10)
        self.emails = []

    async def get_next_with_timeout(self):
        async with real_clock_timeout():
            return await self.get_next()

    async def get_next(self):
        return await self._recv_email.receive()

    def _push(self, to_addr, message):
        email = (to_addr, message)
        self._send_email.send_nowait(email)
        self.emails.append(email)


@pytest.fixture
def email_letterbox(monkeypatch):
    letterbox = LetterBox()

    async def _mocked_send_email(email_config, to_addr, message):
        letterbox._push(to_addr, message)

    monkeypatch.setattr("parsec.backend.invite.send_email", _mocked_send_email)
    return letterbox


@pytest.fixture
def webhook_spy(monkeypatch):
    events = []

    class MockedRep:
        @property
        def status(self):
            return 200

    @contextmanager
    def _mock_urlopen(req, **kwargs):
        # Webhook are alway POST with utf-8 JSON body
        assert req.method == "POST"
        assert req.headers == {"Content-type": "application/json; charset=utf-8"}
        cooked_data = json.loads(req.data.decode("utf-8"))
        events.append((req.full_url, cooked_data))
        yield MockedRep()

    monkeypatch.setattr("parsec.backend.webhooks.urlopen", _mock_urlopen)
    return events


@pytest.fixture
def running_backend_ready(request):
    # Useful to synchronize other fixtures that need to connect to
    # the backend if it is available
    event = trio.Event()
    # Nothing to wait if current test doesn't use `running_backend` fixture
    if "running_backend" not in request.fixturenames:
        event.set()

    return event


@pytest.fixture
async def running_backend(server_factory, backend_addr, backend, running_backend_ready):

    async with server_factory(backend.handle_client, backend_addr) as server:
        server.backend = backend

        def _offline_for(device_id):
            return offline(addr_with_device_subdomain(server.addr, device_id))

        server.offline_for = _offline_for

        running_backend_ready.set()
        yield server


@pytest.fixture
def core_config(tmpdir, backend_addr, unused_tcp_port, fixtures_customization):
    if fixtures_customization.get("fake_preferred_org_creation_backend_addr", False):
        backend_addr = BackendAddr.from_url(f"parsec://localhost:{unused_tcp_port}")

    tmpdir = Path(tmpdir)
    return CoreConfig(
        config_dir=tmpdir / "config",
        data_base_dir=tmpdir / "data",
        mountpoint_base_dir=tmpdir / "mnt",
        preferred_org_creation_backend_addr=backend_addr,
        gui_language=fixtures_customization.get("gui_language"),
    )


@pytest.fixture
def core_factory(request, running_backend_ready, event_bus_factory, core_config):
    @asynccontextmanager
    async def _core_factory(device, event_bus=None):
        # Ensure test doesn't stay frozen if a bug in a fixture prevent the
        # backend from starting
        async with real_clock_timeout():
            await running_backend_ready.wait()
        event_bus = event_bus or event_bus_factory()

        with event_bus.listen() as spy:
            async with logged_core_factory(core_config, device, event_bus) as core:
                # On startup core is always considered offline.
                # Hence we risk concurrency issues if the connection to backend
                # switches online concurrently with the test.
                if "running_backend" in request.fixturenames:
                    await spy.wait_with_timeout(
                        CoreEvent.BACKEND_CONNECTION_CHANGED,
                        {"status": BackendConnStatus.READY, "status_exc": spy.ANY},
                    )
                assert core.are_monitors_idle()

                yield core

    return _core_factory


@pytest.fixture
async def alice_core(
    core_config, fixtures_customization, initialize_local_user_manifest, core_factory, alice
):
    initial_user_manifest = fixtures_customization.get("alice_initial_local_user_manifest", "v1")
    await initialize_local_user_manifest(
        core_config.data_base_dir, alice, initial_user_manifest=initial_user_manifest
    )
    async with core_factory(alice) as core:
        yield core


@pytest.fixture
async def alice2_core(
    core_config, fixtures_customization, initialize_local_user_manifest, core_factory, alice2
):
    initial_user_manifest = fixtures_customization.get("alice2_initial_local_user_manifest", "v1")
    await initialize_local_user_manifest(
        core_config.data_base_dir, alice2, initial_user_manifest=initial_user_manifest
    )
    async with core_factory(alice2) as core:
        yield core


@pytest.fixture
async def otheralice_core(core_config, initialize_local_user_manifest, core_factory, otheralice):
    await initialize_local_user_manifest(
        core_config.data_base_dir, otheralice, initial_user_manifest="v1"
    )
    async with core_factory(otheralice) as core:
        yield core


@pytest.fixture
async def adam_core(
    core_config, fixtures_customization, initialize_local_user_manifest, core_factory, adam
):
    initial_user_manifest = fixtures_customization.get("adam_initial_local_user_manifest", "v1")
    await initialize_local_user_manifest(
        core_config.data_base_dir, adam, initial_user_manifest=initial_user_manifest
    )
    async with core_factory(adam) as core:
        yield core


@pytest.fixture
async def bob_core(
    core_config, fixtures_customization, initialize_local_user_manifest, core_factory, bob
):
    initial_user_manifest = fixtures_customization.get("bob_initial_local_user_manifest", "v1")
    await initialize_local_user_manifest(
        core_config.data_base_dir, bob, initial_user_manifest=initial_user_manifest
    )
    async with core_factory(bob) as core:
        yield core
