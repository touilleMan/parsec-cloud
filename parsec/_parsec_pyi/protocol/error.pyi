from __future__ import annotations

class ProtocolErrorFields:
    @classmethod
    def NotHandled(cls) -> ProtocolErrorFields: ...
    @classmethod
    def BadRequest(cls, exc: str) -> ProtocolErrorFields: ...
    @property
    def exc(self) -> ProtocolErrorFields: ...

class ProtocolError(BaseException, ProtocolErrorFields): ...
