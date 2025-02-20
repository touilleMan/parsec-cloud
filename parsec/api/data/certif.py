# Parsec Cloud (https://parsec.cloud) Copyright (c) AGPL-3.0 2016-present Scille SAS
from __future__ import annotations

from typing import Any, Dict, Literal

import attr

from parsec._parsec import DateTime
from parsec.api.data.base import BaseAPIData, BaseAPISignedData, BaseSignedDataSchema
from parsec.api.protocol import SequesterServiceID, SequesterServiceIDField
from parsec.sequester_crypto import SequesterEncryptionKeyDer, SequesterVerifyKeyDer
from parsec.serde import fields, post_load
from parsec.serde.schema import BaseSchema


@attr.s(slots=True, frozen=True, auto_attribs=True, kw_only=True, eq=False)
class SequesterAuthorityCertificate(BaseAPISignedData):
    class SCHEMA_CLS(BaseSignedDataSchema):
        type = fields.CheckedConstant("sequester_authority_certificate", required=True)
        # Override author field to always uses None given this certificate can only be signed by the root key
        author = fields.CheckedConstant(  # type: ignore[assignment]
            None, required=True, allow_none=True
        )  # Constant None fields required to be allowed to be None !
        verify_key_der = fields.SequesterVerifyKeyDerField(required=True)

        @post_load
        def make_obj(self, data: Dict[str, Any]) -> "SequesterAuthorityCertificate":  # type: ignore[misc]
            data.pop("type")
            return SequesterAuthorityCertificate(**data)

    # Override author field to always uses None given this certificate can only be signed by the root key
    author: Literal[None]  # type: ignore[assignment]
    verify_key_der: SequesterVerifyKeyDer


@attr.s(slots=True, frozen=True, auto_attribs=True, kw_only=True, eq=False)
class SequesterServiceCertificate(BaseAPIData):
    class SCHEMA_CLS(BaseSchema):
        # No author field here given we are signed by the sequester authority
        type = fields.CheckedConstant("sequester_service_certificate", required=True)
        timestamp = fields.DateTime(required=True)
        service_id = SequesterServiceIDField(required=True)
        service_label = fields.String(required=True)
        encryption_key_der = fields.SequesterEncryptionKeyDerField(required=True)

        @post_load
        def make_obj(self, data: Dict[str, Any]) -> "SequesterServiceCertificate":  # type: ignore[misc]
            data.pop("type")
            return SequesterServiceCertificate(**data)

    timestamp: DateTime
    service_id: SequesterServiceID
    service_label: str
    encryption_key_der: SequesterEncryptionKeyDer
