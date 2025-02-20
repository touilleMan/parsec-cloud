# Parsec Cloud (https://parsec.cloud) Copyright (c) AGPL-3.0 2016-present Scille SAS

from __future__ import annotations

from typing import Tuple

from parsec._parsec_pyi.addrs import BackendOrganizationAddr
from parsec._parsec_pyi.crypto import PrivateKey, PublicKey, SecretKey, SigningKey, VerifyKey
from parsec._parsec_pyi.enumerate import UserProfile
from parsec._parsec_pyi.ids import (
    DeviceID,
    DeviceLabel,
    DeviceName,
    EntryID,
    HumanHandle,
    OrganizationID,
    UserID,
)
from parsec._parsec_pyi.time import DateTime, TimeProvider

class LocalDevice:
    def __init__(
        self,
        organization_addr: BackendOrganizationAddr,
        device_id: DeviceID,
        device_label: DeviceLabel | None,
        human_handle: HumanHandle | None,
        signing_key: SigningKey,
        private_key: PrivateKey,
        profile: UserProfile,
        user_manifest_id: EntryID,
        user_manifest_key: SecretKey,
        local_symkey: SecretKey,
    ) -> None: ...
    def evolve(
        self,
        organization_addr: BackendOrganizationAddr | None,
        device_id: DeviceID | None,
        device_label: DeviceLabel | None,
        human_handle: HumanHandle | None,
        signing_key: SigningKey | None,
        private_key: PrivateKey | None,
        profile: UserProfile | None,
        user_manifest_id: EntryID | None,
        user_manifest_key: SecretKey | None,
        local_symkey: SecretKey | None,
    ) -> LocalDevice: ...
    @property
    def is_admin(self) -> bool: ...
    @property
    def is_outsider(self) -> bool: ...
    @property
    def slug(self) -> str: ...
    @property
    def slughash(self) -> str: ...
    @property
    def root_verify_key(self) -> VerifyKey: ...
    @property
    def organization_id(self) -> OrganizationID: ...
    @property
    def device_name(self) -> DeviceName: ...
    @property
    def user_id(self) -> UserID: ...
    @property
    def verify_key(self) -> VerifyKey: ...
    @property
    def public_key(self) -> PublicKey: ...
    @property
    def user_display(self) -> str: ...
    @property
    def short_user_display(self) -> str: ...
    @property
    def device_display(self) -> str: ...
    @property
    def organization_addr(self) -> BackendOrganizationAddr: ...
    @property
    def device_id(self) -> DeviceID: ...
    @property
    def device_label(self) -> DeviceLabel | None: ...
    @property
    def human_handle(self) -> HumanHandle | None: ...
    @property
    def signing_key(self) -> SigningKey: ...
    @property
    def private_key(self) -> PrivateKey: ...
    @property
    def profile(self) -> UserProfile: ...
    @property
    def user_manifest_id(self) -> EntryID: ...
    @property
    def user_manifest_key(self) -> SecretKey: ...
    @property
    def local_symkey(self) -> SecretKey: ...
    @property
    def time_provider(self) -> TimeProvider: ...
    def timestamp(self) -> DateTime: ...
    def dump(self) -> bytes: ...
    @classmethod
    def load_slug(cls, slug: str) -> Tuple[OrganizationID, DeviceID]: ...
    @classmethod
    def load(cls, encrypted: bytes) -> LocalDevice: ...

class UserInfo:
    def __init__(
        self,
        user_id: UserID,
        human_handle: HumanHandle | None,
        profile: UserProfile,
        created_on: DateTime,
        revoked_on: DateTime | None,
    ) -> None: ...
    def __lt__(self, other: UserInfo) -> bool: ...
    def __gt__(self, other: UserInfo) -> bool: ...
    def __le__(self, other: UserInfo) -> bool: ...
    def __ge__(self, other: UserInfo) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def user_id(self) -> UserID: ...
    @property
    def human_handle(self) -> HumanHandle | None: ...
    @property
    def profile(self) -> UserProfile: ...
    @property
    def created_on(self) -> DateTime: ...
    @property
    def revoked_on(self) -> DateTime | None: ...
    @property
    def user_display(self) -> str: ...
    @property
    def short_user_display(self) -> str: ...
    @property
    def is_revoked(self) -> bool: ...

class DeviceInfo:
    def __init__(
        self,
        device_id: DeviceID,
        device_label: DeviceLabel | None,
        created_on: DateTime,
    ) -> None: ...
    def __lt__(self, other: DeviceInfo) -> bool: ...
    def __gt__(self, other: DeviceInfo) -> bool: ...
    def __le__(self, other: DeviceInfo) -> bool: ...
    def __ge__(self, other: DeviceInfo) -> bool: ...
    def __hash__(self) -> int: ...
    @property
    def device_id(self) -> DeviceID: ...
    @property
    def device_label(self) -> DeviceLabel | None: ...
    @property
    def created_on(self) -> DateTime: ...
    @property
    def device_name(self) -> DeviceName: ...
    @property
    def device_display(self) -> str: ...
