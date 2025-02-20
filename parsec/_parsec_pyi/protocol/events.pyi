# Parsec Cloud (https://parsec.cloud) Copyright (c) AGPL-3.0 2016-present Scille SAS

from __future__ import annotations

from parsec._parsec import InvitationStatus, InvitationToken, RealmID, RealmRole, VlobID

# Events
class EventsListenReq:
    def __init__(self, wait: bool) -> None: ...
    @property
    def wait(self) -> bool: ...

class EventsListenRep:
    def dump(self) -> bytes: ...
    @classmethod
    def load(cls, buf: bytes) -> EventsListenRep: ...

class EventsListenRepOk(EventsListenRep): ...

class EventsListenRepOkPinged(EventsListenRep):
    def __init__(self, ping: str) -> None: ...
    @property
    def ping(self) -> str: ...

class EventsListenRepOkMessageReceived(EventsListenRep):
    def __init__(self, index: int) -> None: ...
    @property
    def index(self) -> int: ...

class EventsListenRepOkInviteStatusChanged(EventsListenRep):
    def __init__(self, token: InvitationToken, invitation_status: InvitationStatus) -> None: ...
    @property
    def token(self) -> InvitationToken: ...
    @property
    def invitation_status(self) -> InvitationStatus: ...

class EventsListenRepOkRealmMaintenanceFinished(EventsListenRep):
    def __init__(self, realm_id: RealmID, encryption_revision: int) -> None: ...
    @property
    def realm_id(self) -> RealmID: ...
    @property
    def encryption_revision(self) -> int: ...

class EventsListenRepOkRealmMaintenanceStarted(EventsListenRep):
    def __init__(self, realm_id: RealmID, encryption_revision: int) -> None: ...
    @property
    def realm_id(self) -> RealmID: ...
    @property
    def encryption_revision(self) -> int: ...

class EventsListenRepOkRealmVlobsUpdated(EventsListenRep):
    def __init__(
        self, realm_id: RealmID, checkpoint: int, src_id: VlobID, src_version: int
    ) -> None: ...
    @property
    def realm_id(self) -> RealmID: ...
    @property
    def checkpoint(self) -> int: ...
    @property
    def src_id(self) -> VlobID: ...
    @property
    def src_version(self) -> int: ...

class EventsListenRepOkRealmRolesUpdated(EventsListenRep):
    def __init__(self, real_id: RealmID, role: RealmRole) -> None: ...
    @property
    def realm_id(self) -> RealmID: ...
    @property
    def role(self) -> RealmRole: ...

class EventsListenRepCancelled(EventsListenRep):
    def __init__(self, reason: str | None) -> None: ...
    @property
    def reason(self) -> str | None: ...

class EventsListenRepOkPkiEnrollmentUpdated(EventsListenRep): ...
class EventsListenRepNoEvents(EventsListenRep): ...

class EventsListenRepUnknownStatus(EventsListenRep):
    @property
    def status(self) -> str: ...
    @property
    def reason(self) -> str | None: ...

class EventsSubscribeReq: ...

class EventsSubscribeRep:
    def dump(self) -> bytes: ...
    @classmethod
    def load(cls, buf: bytes) -> EventsSubscribeRep: ...

class EventsSubscribeRepOk(EventsSubscribeRep): ...

class EventsSubscribeRepUnknownStatus(EventsSubscribeRep):
    @property
    def status(self) -> str: ...
    @property
    def reason(self) -> str | None: ...
