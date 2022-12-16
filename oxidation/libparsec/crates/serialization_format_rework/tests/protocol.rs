// Parsec Cloud (https://parsec.cloud) Copyright (c) BUSL-1.1 (eventually AGPL-3.0) 2016-present Scille SAS

use libparsec_types::Maybe;
use pretty_assertions::assert_eq;

use libparsec_serialization_format::generate_protocol_familly_from_contents;

#[test]
fn test_simple() {
    generate_protocol_familly_from_contents!(
        r#"[
    {
        "major_versions": [
            1,
            2,
            3
        ],
        "req": {
            "cmd": "ping",
            "other_fields": [
                {
                    "name": "ping",
                    "type": "String"
                }
            ]
        },
        "reps": [
            {
                "status": "ok",
                "other_fields": [
                    {
                        "name": "pong",
                        "type": "String"
                    }
                ]
            }
        ]
    }
]
"#
    );

    // Check v1/v2/v3 use the same structure (this won't compile if not)

    assert_eq!(
        protocol::v1::ping::Req {
            ping: "foo".to_owned()
        },
        protocol::v2::ping::Req {
            ping: "foo".to_owned()
        }
    );
    assert_eq!(
        protocol::v1::ping::Req {
            ping: "foo".to_owned()
        },
        protocol::v3::ping::Req {
            ping: "foo".to_owned()
        }
    );

    // Check round-trip seriliaze/deserialize

    let req = protocol::v2::ping::Req {
        ping: "foo".to_owned(),
    };
    let dumped = req.clone().dump().unwrap();
    assert_eq!(
        protocol::v2::AnyCmdReq::load(&dumped).unwrap(),
        protocol::v2::AnyCmdReq::Ping(req)
    );

    let rep = protocol::v2::ping::Rep::Ok {
        pong: "foo".to_owned(),
    };
    let dumped = rep.clone().dump().unwrap();
    assert_eq!(
        protocol::v2::ping::Rep::load(&dumped).unwrap(),
        protocol::v2::ping::Rep::Ok {
            pong: "foo".to_owned()
        }
    );
}

#[test]
fn test_unknown_rep_status() {
    generate_protocol_familly_from_contents!(
        r#"[
    {
        "major_versions": [1],
        "req": {
            "cmd": "ping",
            "other_fields": []
        },
        "reps": [
            {
                "status": "ok",
                "other_fields": [
                    {
                        "name": "pong",
                        "type": "String"
                    }
                ]
            }
        ]
    }
]

[
    {
        "major_versions": [1],
        "req": {
            "cmd": "ping2",
            "other_fields": []
        },
        "reps": [
            {
                "status": "ok",
                "other_fields": [
                    {
                        "name": "dummy",
                        "type": "String"
                    }
                ]
            },
            {
                "status": "dummy",
                "other_fields": [
                    {
                        "name": "pong",
                        "type": "String"
                    }
                ]
            }
        ]
    }
]
"#
    );

    let unknown_status = protocol::v1::ping2::Rep::Dummy {
        pong: "foo".to_owned(),
    }
    .dump()
    .unwrap();
    let known_status_but_bad_content = protocol::v1::ping2::Rep::Ok {
        dummy: "foo".to_owned(),
    }
    .dump()
    .unwrap();

    assert_eq!(
        protocol::v1::ping::Rep::load(&unknown_status).unwrap(),
        protocol::v1::ping::Rep::UnknownStatus {
            unknown_status: "dummy".to_owned(),
            reason: None
        }
    );
    assert!(protocol::v1::ping::Rep::load(&known_status_but_bad_content).is_err());
}

#[test]
fn test_introduce_in_field() {
    generate_protocol_familly_from_contents!(
        r#"[
    {
        "major_versions": [
            1,
            2,
            3
        ],
        "req": {
            "cmd": "ping",
            "other_fields": [
                {
                    "name": "ping",
                    "type": "String",
                    "introduced_in": "APIv2.1"
                }
            ]
        },
        "reps": []
    }
]

[
    {
        "major_versions": [
            1,
            2,
            3
        ],
        "req": {
            "cmd": "ping2",
            "other_fields": []
        },
        "reps": [
            {
                "status": "ok",
                "other_fields": [
                    {
                        "name": "pong",
                        "type": "String",
                        "introduced_in": "APIv2.1"
                    }
                ]
            }
        ]
    }
]
"#
    );

    // Test Req

    let v1 = protocol::v1::ping::Req {};
    let v2_with = protocol::v2::ping::Req {
        ping: Maybe::Present("foo".to_owned()),
    };
    let v2_without = protocol::v2::ping::Req {
        ping: Maybe::Absent,
    };
    let v3 = protocol::v3::ping::Req {
        ping: "foo".to_owned(),
    };

    let v1_dumped = v1.clone().dump().unwrap();
    let v2_with_dumped = v2_with.clone().dump().unwrap();
    // Field is optional in v2...
    assert_eq!(
        protocol::v2::AnyCmdReq::load(&v1_dumped).unwrap(),
        protocol::v2::AnyCmdReq::Ping(v2_without.clone())
    );
    // ...and becomes required in v3
    assert!(protocol::v3::AnyCmdReq::load(&v1_dumped).is_err());
    assert_eq!(
        protocol::v3::AnyCmdReq::load(&v2_with_dumped).unwrap(),
        protocol::v3::AnyCmdReq::Ping(v3)
    );

    // Test Rep

    let v1 = protocol::v1::ping2::Rep::Ok {};
    let v2_with = protocol::v2::ping2::Rep::Ok {
        pong: Maybe::Present("foo".to_owned()),
    };
    let v2_without = protocol::v2::ping2::Rep::Ok {
        pong: Maybe::Absent,
    };
    let v3 = protocol::v3::ping2::Rep::Ok {
        pong: "foo".to_owned(),
    };

    let v1_dumped = v1.clone().dump().unwrap();
    let v2_with_dumped = v2_with.clone().dump().unwrap();
    // Field is optional in v2...
    assert_eq!(
        protocol::v2::ping2::Rep::load(&v1_dumped).unwrap(),
        v2_without.clone()
    );
    // ...and becomes required in v3
    assert!(protocol::v3::ping2::Rep::load(&v1_dumped).is_err());
    assert_eq!(protocol::v3::ping2::Rep::load(&v2_with_dumped).unwrap(), v3);
}
