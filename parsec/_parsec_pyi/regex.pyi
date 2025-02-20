# Parsec Cloud (https://parsec.cloud) Copyright (c) AGPL-3.0 2016-present Scille SAS

from __future__ import annotations

class Regex:
    @classmethod
    def from_pattern(cls, pattern: str) -> Regex: ...
    @classmethod
    def from_regex_str(cls, regex_str: str) -> Regex: ...
    @property
    def pattern(self) -> str: ...
