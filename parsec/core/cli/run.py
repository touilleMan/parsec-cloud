# Parsec Cloud (https://parsec.cloud) Copyright (c) AGPLv3 2016-2021 Scille SAS

import trio
import click
from pathlib import Path
from pendulum import DateTime, parse as pendulum_parse

from parsec.utils import trio_run
from parsec.logging import configure_sentry_logging
from parsec.cli_utils import (
    cli_exception_handler,
    generate_not_available_cmd,
    sentry_config_options,
)
from parsec.core import logged_core_factory
from parsec.core.cli.utils import core_config_and_device_options, core_config_options

try:
    from parsec.core.gui import run_gui as _run_gui

except ImportError as exc:
    run_gui = generate_not_available_cmd(exc)

else:

    @click.command(short_help="run parsec GUI")
    # Let the GUI handle the parsing of the url to display dialog on error
    @click.argument("url", required=False)
    @click.option("--diagnose", "-d", is_flag=True)
    @core_config_options
    # Add --sentry-url
    @sentry_config_options(configure_sentry=False)
    def run_gui(config, url, diagnose, sentry_url, **kwargs):
        """
        Run parsec GUI
        """
        if config.telemetry_enabled and sentry_url:
            configure_sentry_logging(sentry_url)

        config = config.evolve(mountpoint_enabled=True)
        _run_gui(config, start_arg=url, diagnose=diagnose)


async def _run_mountpoint(config, device, timestamp: DateTime = None):
    config = config.evolve(mountpoint_enabled=True)
    async with logged_core_factory(config, device):
        display_device = click.style(device.device_id, fg="yellow")
        mountpoint_display = click.style(str(config.mountpoint_base_dir.absolute()), fg="yellow")
        click.echo(f"{display_device}'s drive mounted at {mountpoint_display}")

        await trio.sleep_forever()


@click.command(short_help="run parsec mountpoint")
@click.option("--mountpoint", "-m", type=click.Path(exists=False))
@click.option("--timestamp", "-t", type=lambda t: pendulum_parse(t, tz="local"))
@core_config_and_device_options
def run_mountpoint(config, device, mountpoint, timestamp, **kwargs):
    """
    Expose device's parsec drive on the given mountpoint.
    """
    config = config.evolve(mountpoint_enabled=True)
    if mountpoint:
        config = config.evolve(mountpoint_base_dir=Path(mountpoint))
    with cli_exception_handler(config.debug):
        trio_run(_run_mountpoint, config, device, timestamp)
