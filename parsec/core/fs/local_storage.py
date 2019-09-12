# Parsec Cloud (https://parsec.cloud) Copyright (c) AGPLv3 2019 Scille SAS

from pathlib import Path
from collections import defaultdict
import trio
from trio import hazmat
from typing import Dict, Tuple, Set

from pendulum import Pendulum
from structlog import get_logger
from async_generator import asynccontextmanager

from parsec.crypto import SecretKey
from parsec.api.protocol import DeviceID
from parsec.core.types import (
    EntryID,
    BlockID,
    ChunkID,
    FileDescriptor,
    LocalManifest,
    LocalFileManifest,
)
from parsec.core.fs.persistent_storage import PersistentStorage
from parsec.core.fs.exceptions import FSError, FSLocalMissError, FSInvalidFileDescriptor


logger = get_logger()


class LocalStorage:
    """Manage the access to the local storage.

    That includes:
    - a cache in memory for fast access to deserialized data
    - the persistent storage to keep serialized data on the disk
    - a lock mecanism to protect against race conditions
    """

    persistent_storage_class = PersistentStorage

    def __init__(self, device_id: DeviceID, key: SecretKey, path: Path, **kwargs):
        self.device_id = device_id

        # File descriptors
        self.open_fds: Dict[FileDescriptor, EntryID] = {}
        self.fd_counter = 0

        # Locking structures
        self.locking_tasks = {}
        self.entry_locks = defaultdict(trio.Lock)

        # Manifest and block storage
        self.cache_ahead_of_persistance_ids = set()
        self.local_manifest_cache = {}
        self.persistent_storage = self.persistent_storage_class(key, path, **kwargs)

    def _get_next_fd(self) -> FileDescriptor:
        self.fd_counter += 1
        return FileDescriptor(self.fd_counter)

    async def __aenter__(self):
        self.persistent_storage.__enter__()
        return self

    async def __aexit__(self, *args):
        if self.locking_tasks:
            raise RuntimeError("Cannot teardown while entries are still locked")
        for entry_id in self.cache_ahead_of_persistance_ids.copy():
            await self._ensure_manifest_persistent(entry_id)
        self.persistent_storage.__exit__(*args)

    def clear_memory_cache(self):
        self.local_manifest_cache.clear()
        self.cache_ahead_of_persistance_ids.clear()

    # Locking helpers

    @asynccontextmanager
    async def lock_entry_id(self, entry_id: EntryID):
        async with self.entry_locks[entry_id]:
            try:
                self.locking_tasks[entry_id] = hazmat.current_task()
                yield entry_id
            finally:
                del self.locking_tasks[entry_id]

    @asynccontextmanager
    async def lock_manifest(self, entry_id: EntryID):
        async with self.lock_entry_id(entry_id):
            yield await self.get_manifest(entry_id)

    def _check_lock_status(self, entry_id: EntryID) -> None:
        task = self.locking_tasks.get(entry_id)
        if task != hazmat.current_task():
            raise RuntimeError(f"Entry `{entry_id}` modified without beeing locked")

    # Manifest interface

    async def get_realm_checkpoint(self) -> int:
        return self.persistent_storage.get_realm_checkpoint()

    async def update_realm_checkpoint(
        self, new_checkpoint: int, changed_vlobs: Dict[EntryID, int]
    ) -> None:
        """
        Raises: Nothing !
        """
        self.persistent_storage.update_realm_checkpoint(new_checkpoint, changed_vlobs)

    async def get_need_sync_entries(self) -> Tuple[Set[EntryID], Set[EntryID]]:
        return self.persistent_storage.get_need_sync_entries()

    async def get_manifest(self, entry_id: EntryID) -> LocalManifest:
        """Raises: FSLocalMissError"""
        assert isinstance(entry_id, EntryID)
        try:
            return self.local_manifest_cache[entry_id]
        except KeyError:
            pass
        manifest = self.persistent_storage.get_manifest(entry_id)
        self.local_manifest_cache[entry_id] = manifest
        return manifest

    async def set_manifest(
        self,
        entry_id: EntryID,
        manifest: LocalManifest,
        cache_only: bool = False,
        check_lock_status=True,
    ) -> None:
        assert isinstance(entry_id, EntryID)
        if check_lock_status:
            self._check_lock_status(entry_id)
        if not cache_only:
            self.persistent_storage.set_manifest(entry_id, manifest)
        else:
            self.cache_ahead_of_persistance_ids.add(entry_id)
        self.local_manifest_cache[entry_id] = manifest

    async def ensure_manifest_persistent(self, entry_id: EntryID) -> None:
        assert isinstance(entry_id, EntryID)
        self._check_lock_status(entry_id)
        if entry_id not in self.cache_ahead_of_persistance_ids:
            return
        await self._ensure_manifest_persistent(entry_id)

    async def _ensure_manifest_persistent(self, entry_id: EntryID) -> None:
        manifest = self.local_manifest_cache[entry_id]
        self.persistent_storage.set_manifest(entry_id, manifest)
        self.cache_ahead_of_persistance_ids.remove(entry_id)

    async def clear_manifest(self, entry_id: EntryID) -> None:
        assert isinstance(entry_id, EntryID)
        self._check_lock_status(entry_id)
        try:
            self.persistent_storage.clear_manifest(entry_id)
        except FSLocalMissError:
            pass
        self.local_manifest_cache.pop(entry_id, None)
        self.cache_ahead_of_persistance_ids.discard(entry_id)

    # Clean block interface

    async def is_clean_block(self, block_id: BlockID):
        assert isinstance(block_id, BlockID)
        return not self.persistent_storage.is_dirty_chunk(block_id)

    async def set_clean_block(self, block_id: BlockID, block: bytes) -> None:
        assert isinstance(block_id, BlockID)
        return self.persistent_storage.set_clean_block(block_id, block)

    async def clear_clean_block(self, block_id: BlockID) -> None:
        assert isinstance(block_id, BlockID)
        try:
            self.persistent_storage.clear_clean_block(block_id)
        except FSLocalMissError:
            pass

    async def get_dirty_block(self, block_id: BlockID) -> bytes:
        return self.persistent_storage.get_dirty_chunk(ChunkID(block_id))

    # Chunk interface

    async def get_chunk(self, chunk_id: ChunkID) -> bytes:
        assert isinstance(chunk_id, ChunkID)
        try:
            return self.persistent_storage.get_dirty_chunk(chunk_id)
        except FSLocalMissError:
            return self.persistent_storage.get_clean_block(chunk_id)

    async def set_chunk(self, chunk_id: ChunkID, block: bytes) -> None:
        assert isinstance(chunk_id, ChunkID)
        return self.persistent_storage.set_dirty_chunk(chunk_id, block)

    async def clear_chunk(self, chunk_id: ChunkID, miss_ok: bool = False) -> None:
        assert isinstance(chunk_id, ChunkID)
        try:
            self.persistent_storage.clear_dirty_chunk(chunk_id)
        except FSLocalMissError:
            if not miss_ok:
                raise

    # File management interface

    def create_file_descriptor(self, manifest: LocalFileManifest) -> FileDescriptor:
        assert isinstance(manifest, LocalFileManifest)
        fd = self._get_next_fd()
        self.open_fds[fd] = manifest.id
        return fd

    async def load_file_descriptor(self, fd: FileDescriptor) -> LocalFileManifest:
        try:
            entry_id = self.open_fds[fd]
        except KeyError:
            raise FSInvalidFileDescriptor(fd)
        manifest = await self.get_manifest(entry_id)
        assert isinstance(manifest, LocalFileManifest)
        return manifest

    def remove_file_descriptor(self, fd: FileDescriptor) -> None:
        try:
            self.open_fds.pop(fd)
        except KeyError:
            raise FSInvalidFileDescriptor(fd)

    # Vacuum

    async def run_vacuum(self):
        self.persistent_storage.run_vacuum()

    # Timestamped workspace

    def to_timestamped(self, timestamp: Pendulum):
        return LocalStorageTimestamped(self, timestamp)


class LocalStorageTimestamped(LocalStorage):
    """Timestamped version to access a local storage as it was at a given timestamp

    That includes:
    - another cache in memory for fast access to deserialized data
    - the timestamped persistent storage to keep serialized data on the disk :
      vlobs are in common, not manifests. Actually only vlobs are used, manifests are mocked
    - the same lock mecanism to protect against race conditions, although it is useless there
    """

    def __init__(self, local_storage: LocalStorage, timestamp: Pendulum):
        super().__init__(local_storage.device_id, None, "")
        self.persistent_storage = local_storage.persistent_storage

        self.set_chunk = self._throw_permission_error
        self.clean_chunk = self._throw_permission_error

    def _throw_permission_error(*args, **kwargs):
        raise FSError("Not implemented : LocalStorage is timestamped")

    # Manifest interface

    async def get_manifest(self, entry_id: EntryID) -> LocalManifest:
        """Raises: FSLocalMissError"""
        assert isinstance(entry_id, EntryID)
        try:
            return self.local_manifest_cache[entry_id]
        except KeyError:
            raise FSLocalMissError(entry_id)

    async def set_manifest(
        self, entry_id: EntryID, manifest: LocalManifest, cache_only: bool = False
    ) -> None:  # initially for clean
        assert isinstance(entry_id, EntryID)
        if manifest.need_sync:
            return self._throw_permission_error()
        self._check_lock_status(entry_id)
        self.local_manifest_cache[entry_id] = manifest

    async def clear_manifest(self, entry_id: EntryID) -> None:
        assert isinstance(entry_id, EntryID)
        self._check_lock_status(entry_id)
        self.local_manifest_cache.pop(entry_id, None)

    async def ensure_manifest_persistent(self, entry_id: EntryID) -> None:
        pass
