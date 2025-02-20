// Parsec Cloud (https://parsec.cloud) Copyright (c) BUSL-1.1 (eventually AGPL-3.0) 2016-present Scille SAS

mod error;
mod local_device;
mod local_device_file;
mod local_manifest;

pub use error::*;
pub use local_device::*;
pub use local_device_file::*;
pub use local_manifest::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClientType {
    Authenticated,
    Invited,
    Anonymous,
    Apiv1Anonymous,
    Apiv1Administration,
}
