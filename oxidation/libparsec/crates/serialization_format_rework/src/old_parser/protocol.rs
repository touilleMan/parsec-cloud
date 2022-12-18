// Parsec Cloud (https://parsec.cloud) Copyright (c) BUSL-1.1 (eventually AGPL-3.0) 2016-present Scille SAS

use itertools::Itertools;
use miniserde::Deserialize;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashSet;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::old_parser::utils::inspect_type;

use super::utils::quote_serde_as;

// TODO: move me in utils
pub(crate) fn snake_to_camel_case(s: &str) -> String {
    let mut out = s[..1].to_uppercase();
    let mut chars = s.chars().skip(1);
    while let Some(c) = chars.next() {
        if c == '_' {
            match chars.next().unwrap_or_else(|| unreachable!()) {
                c @ 'a'..='z' => out.push((c as u8 - b'a' + b'A') as char),
                c => out.push(c),
            }
        } else {
            out.push(c);
        }
    }

    out
}

fn parse_api_version(raw: &str) -> Result<(u32, u32), &'static str> {
    let err_msg = "Bad API version, must be e.g. `APIv1.2`";
    if !raw.starts_with("APIv") {
        return Err(err_msg);
    }
    match raw.split_once("APIv") {
        Some(("", major_minor)) => {
            // let (major, minor): (u32, u32) =
            let mut splitted = major_minor.splitn(2, ".");
            let major = splitted.next().and_then(|x| x.parse::<u32>().ok());
            let minor = splitted.next().and_then(|x| x.parse::<u32>().ok());
            match (major, minor, splitted.next()) {
                (Some(major), Some(minor), None) => Ok((major, minor)),
                _ => Err(err_msg),
            }
        }
        _ => Err(err_msg),
    }
}

// TODO: handle nested type, unit type etc.

//
// JSON format schemas
//

#[derive(Deserialize)]
pub(crate) struct JsonCmd {
    // Miniserde only supports struct as root, hence this field that is never
    // present in the actual json files: it will be added just before
    // deserialization by the parsec_protocol_familly macro !
    items: Vec<JsonCmdFlavour>,
}

#[derive(Deserialize)]
pub(crate) struct JsonCmdFlavour {
    major_versions: Vec<u32>,
    req: JsonCmdReq,
    reps: Vec<JsonCmdRep>,
    nested_types: Option<Vec<JsonNestedType>>,
}

#[derive(Deserialize)]
pub(crate) enum JsonNestedTypeKind {
    #[serde(rename = "enum")]
    Enum,
    #[serde(rename = "struct")]
    Struct,
}

#[derive(Deserialize)]
pub(crate) struct JsonNestedTypeVariant {
    name: String,
    discriminant_value: String,
    fields: Vec<JsonCmdField>,
}

#[derive(Deserialize)]
pub(crate) struct JsonNestedType {
    name: String,
    #[serde(rename = "type")]
    ty: JsonNestedTypeKind,
    variants: Option<Vec<JsonNestedTypeVariant>>,
    fields: Option<Vec<JsonCmdField>>,
}

#[derive(Deserialize)]
pub(crate) struct JsonCmdReq {
    cmd: String,
    other_fields: Vec<JsonCmdField>,
    // other_fields: Option<Vec<JsonCmdField>>,
    // unit: Option<String>,
}

#[derive(Deserialize)]
pub(crate) struct JsonCmdRep {
    status: String,
    other_fields: Vec<JsonCmdField>,
}

#[derive(Deserialize)]
pub(crate) struct JsonCmdField {
    name: String,
    #[serde(rename = "type")]
    ty: String,
    // In which API version the current field was introduced.
    // Must be in `X.Y` format
    introduced_in: Option<String>,
}

//
// Cooked structures that will generate the code
//

pub(crate) struct GenCmd {
    pub cmd: String,
    // TODO: do we also need an `introduced_in` field for command ?
    pub spec: GenCmdSpec,
}

pub(crate) enum GenCmdSpec {
    Original {
        req: GenCmdReq,
        reps: Vec<GenCmdRep>,
        nested_types: Vec<GenCmdNestedType>,
    },
    ReusedFromVersion {
        version: u32,
    },
}

pub(crate) struct GenCmdReq {
    pub _cmd: String,
    pub other_fields: Vec<GenCmdField>,
}

pub(crate) struct GenCmdRep {
    // TODO: do we also need an `introduced_in` field for response status ?
    pub status: String,
    pub other_fields: Vec<GenCmdField>,
}

pub(crate) struct GenCmdField {
    pub name: String,
    pub ty: String,
    // Field is required if the command has always contain it, or if
    // it has been introduced in a previous major version of the API
    pub added_in_minor_revision: bool,
    // Expose what has been provided in `other_fields`
    pub allowed_extra_types: Rc<RefCell<HashMap<String, String>>>,
}

pub(crate) struct GenCmdNestedTypeVariant {
    pub name: String,
    pub discriminant_value: String,
    pub fields: Vec<GenCmdField>,
}

pub(crate) enum GenCmdNestedType {
    Enum {
        name: String,
        variants: Vec<GenCmdNestedTypeVariant>,
    },
    Struct {
        name: String,
        fields: Vec<GenCmdField>,
    },
}

pub(crate) struct GenCmdsFamilly {
    pub name: String,
    pub versions: HashMap<u32, Vec<GenCmd>>,
}

impl GenCmdsFamilly {
    pub fn new(cmds: Vec<JsonCmd>, familly_name: &str) -> Self {
        let mut gen_versions: HashMap<u32, Vec<GenCmd>> = HashMap::new();
        let mut version_cmd_couples: HashSet<(u32, String)> = HashSet::new();

        // Main work here is to "unfactorize the command": we convert a JsonCmdFlavour
        // struct that is maked as `major_versions: [1, 2]` into two separate GenCmd
        // structs, or into a single GenCmd referenced twice if the command doesn't
        // change between API versions.

        for cmd in cmds.into_iter().flat_map(|json_cmd| json_cmd.items) {
            assert!(
                !cmd.major_versions.is_empty(),
                "{:?}: major_versions field cannot be empty !",
                cmd.req.cmd
            );

            // We don't try to play smart here: if `introduced_in` is set
            // somewhere we consider the schema cannot be shared accross
            // versions.
            // Cleaver optimizations are possible but it is not needed
            // for the moment given how few schema with `introduced_in`
            // we have for the moment.
            let has_introduced_in_field = RefCell::new(false);

            let mut can_reuse_schema_from_version: Option<u32> = None;
            for major_version in cmd.major_versions {
                // Now it's time to convert the Json struct into a Gen one !

                // This will be populated once we have parsed `nested_types` field
                let allowed_extra_types = Rc::new(RefCell::new(HashMap::new()));

                let convert_field = |field: &JsonCmdField| {
                    // Field can be omited in a schema if it has been added in a minor
                    // version revision (e.g. field added in APIv1.1, we must allow it
                    // to be missing to still be compatible with APIv1.0)
                    let added_in_minor_revision = match &field.introduced_in {
                        Some(introduced_in) => {
                            has_introduced_in_field.replace(true);
                            match parse_api_version(&introduced_in) {
                                Ok((introduced_in_major, _)) => {
                                    if introduced_in_major > major_version {
                                        // Field has been added in a subsequent major version,
                                        // hence our current major version doesn't know about it !
                                        return None;
                                    }
                                    introduced_in_major == major_version
                                }
                                Err(err) => panic!("{:?}: {:?}", cmd.req.cmd, err),
                            }
                        }
                        None => false,
                    };
                    Some(GenCmdField {
                        name: field.name.clone(),
                        ty: field.ty.clone(),
                        added_in_minor_revision: added_in_minor_revision,
                        allowed_extra_types: allowed_extra_types.clone(),
                    })
                };

                let gen_nested_types = {
                    if let Some(ref nested_types) = cmd.nested_types {
                        nested_types.iter().map(|nested_type| {
                            match nested_type.ty {
                                JsonNestedTypeKind::Enum => {
                                    match (&nested_type.fields, &nested_type.variants) {
                                        (None, Some(variants)) => {
                                            GenCmdNestedType::Enum {
                                                name: nested_type.name.to_owned(),
                                                variants: variants.iter().map(
                                                    |variant| {
                                                        GenCmdNestedTypeVariant {
                                                            name: variant.name.to_owned(),
                                                            discriminant_value: variant.discriminant_value.to_owned(),
                                                            fields: variant.fields
                                                                .iter()
                                                                .filter_map(&convert_field)
                                                                .collect(),
                                                        }
                                                    }
                                                ).collect()
                                            }
                                        },
                                        _ => {
                                            panic!("{:?}::{:?}: Enum nested type requires `variants` field and should not have `fields` field", &cmd.req.cmd, &nested_type.name);
                                        },
                                    }
                                },
                                JsonNestedTypeKind::Struct => {
                                    match (&nested_type.fields, &nested_type.variants) {
                                        (Some(fields), None) => {
                                            GenCmdNestedType::Struct {
                                                name: nested_type.name.to_owned(),
                                                fields: fields
                                                    .iter()
                                                    .filter_map(&convert_field)
                                                    .collect(),
                                            }
                                        },
                                        _ => {
                                            panic!("{:?}::{:?}: Struct nested type requires `fields` field and should not have `variants` field", &cmd.req.cmd, &nested_type.name);
                                        },
                                    }
                                },
                            }
                        }).collect()
                    } else {
                        vec![]
                    }
                };

                // Told you we were going to populate `allowed_extra_types` !
                {
                    let mut allowed_extra_types = allowed_extra_types.borrow_mut();
                    for nested_type in gen_nested_types.iter() {
                        let name = match nested_type {
                            GenCmdNestedType::Enum { name, .. } => name,
                            GenCmdNestedType::Struct { name, .. } => name,
                        };
                        allowed_extra_types.insert(name.to_owned(), name.to_owned());
                    }
                }

                let gen_req = GenCmdReq {
                    _cmd: cmd.req.cmd.to_owned(),
                    other_fields: cmd
                        .req
                        .other_fields
                        .iter()
                        .filter_map(&convert_field)
                        .collect(),
                };

                let gen_reps = cmd
                    .reps
                    .iter()
                    .map(|rep| GenCmdRep {
                        status: rep.status.to_owned(),
                        other_fields: rep.other_fields.iter().filter_map(&convert_field).collect(),
                    })
                    .collect();

                let gen_cmd = match (
                    *has_introduced_in_field.borrow(),
                    can_reuse_schema_from_version,
                ) {
                    (true, _) => {
                        // Never reuse schema that contains `introduced_in` field
                        assert!(can_reuse_schema_from_version.is_none()); // Sanity check
                        GenCmd {
                            cmd: cmd.req.cmd.to_owned(),
                            spec: GenCmdSpec::Original {
                                req: gen_req,
                                reps: gen_reps,
                                nested_types: gen_nested_types,
                            },
                        }
                    }
                    (false, None) => {
                        // First time we see this schema
                        can_reuse_schema_from_version.replace(major_version);
                        GenCmd {
                            cmd: cmd.req.cmd.to_owned(),
                            spec: GenCmdSpec::Original {
                                req: gen_req,
                                reps: gen_reps,
                                nested_types: gen_nested_types,
                            },
                        }
                    }
                    (false, Some(version)) => GenCmd {
                        cmd: cmd.req.cmd.to_owned(),
                        spec: GenCmdSpec::ReusedFromVersion { version },
                    },
                };

                assert!(
                    version_cmd_couples.insert((major_version, cmd.req.cmd.to_owned())),
                    "APIv{:?} has multiple implementations of {:?} !",
                    major_version,
                    cmd.req.cmd
                );
                gen_versions.entry(major_version).or_default().push(gen_cmd);
            }
        }

        Self {
            name: familly_name.to_owned(),
            versions: gen_versions,
        }
    }
}

//
// Code generation
//

impl GenCmdsFamilly {
    pub(crate) fn quote(&self) -> TokenStream {
        let familly_name = format_ident!("{}", &self.name);

        let versioned_cmds: Vec<TokenStream> = self
            .versions
            .iter()
            .sorted_by_key(|(v, _)| *v)
            .map(|(version, cmds)| quote_versioned_cmds(*version, cmds))
            .collect();

        quote! {
            pub mod #familly_name {
                #(#versioned_cmds)*
            }
        }
    }
}

fn quote_versioned_cmds(version: u32, cmds: &[GenCmd]) -> TokenStream {
    let versioned_cmds_mod = format_ident!("v{version}");
    let (any_cmd_req_variants, cmd_structs): (Vec<TokenStream>, Vec<TokenStream>) =
        cmds.iter().map(quote_cmd).unzip();

    quote! {
        pub mod #versioned_cmds_mod {
            #[derive(Debug, Clone, ::serde::Serialize, ::serde::Deserialize, PartialEq, Eq)]
            #[serde(tag = "cmd")]
            pub enum AnyCmdReq {
                #(#any_cmd_req_variants),*
            }

            impl AnyCmdReq {
                pub fn dump(&self) -> Result<Vec<u8>, ::rmp_serde::encode::Error> {
                    ::rmp_serde::to_vec_named(self)
                }

                pub fn load(buf: &[u8]) -> Result<Self, ::rmp_serde::decode::Error> {
                    ::rmp_serde::from_slice(buf)
                }
            }

            #(#cmd_structs)*
        }
    }
}

fn quote_cmd(cmd: &GenCmd) -> (TokenStream, TokenStream) {
    let camel_case_name = &snake_to_camel_case(&cmd.cmd);
    let snake_case_name = &cmd.cmd;

    let variant_name = format_ident!("{}", camel_case_name);
    let module_name = format_ident!("{}", snake_case_name);
    let command_name = snake_case_name;

    let module = cmd.quote();
    let variant = quote! {
        #[serde(rename = #command_name)]
        #variant_name(#module_name::Req)
    };

    (variant, module)
}

impl GenCmd {
    pub(crate) fn quote(&self) -> TokenStream {
        let variant_name = format_ident!("{}", &snake_to_camel_case(&self.cmd));
        let module_name = format_ident!("{}", &self.cmd);
        match &self.spec {
            GenCmdSpec::ReusedFromVersion { version } => {
                let reused_version_module_name = format_ident!("v{}", version);
                quote! {
                    pub mod #module_name {
                        pub use super::super::#reused_version_module_name::#module_name::Req;
                        pub use super::super::#reused_version_module_name::#module_name::Rep;
                    }
                }
            }

            GenCmdSpec::Original {
                req,
                reps,
                nested_types,
            } => {
                let struct_req = quote_cmd_req_struct(req);
                let variants_rep = quote_cmd_rep_variants(reps);
                let nested_types = quote_cmd_nested_types(nested_types);
                let known_rep_statuses: Vec<String> =
                    reps.iter().map(|rep| rep.status.to_owned()).collect();

                quote! {

                    pub mod #module_name {
                        use super::AnyCmdReq;

                        #(#nested_types)*

                        #struct_req

                        impl Req {
                            pub fn dump(self) -> Result<Vec<u8>, ::rmp_serde::encode::Error> {
                                AnyCmdReq::#variant_name(self).dump()
                            }
                        }

                        // Can't derive Eq because some Rep have f64 field
                        #[allow(clippy::derive_partial_eq_without_eq)]
                        #[::serde_with::serde_as]
                        #[derive(Debug, Clone, ::serde::Serialize, ::serde::Deserialize, PartialEq)]
                        #[serde(tag = "status")]
                        pub enum Rep {
                            #(#variants_rep),*
                        }

                        #[derive(::serde::Deserialize, PartialEq)]
                        struct UnknownStatus {
                            status: String,
                            reason: Option<String>
                        }

                        impl Rep {
                            pub fn dump(&self) -> Result<Vec<u8>, ::rmp_serde::encode::Error> {
                                ::rmp_serde::to_vec_named(self)
                            }

                            pub fn load(buf: &[u8]) -> Result<Self, ::rmp_serde::decode::Error> {
                                ::rmp_serde::from_slice::<Self>(buf)
                                    .or_else(|err| {
                                        // Due to how Serde handles variant discriminant, we cannot express unknown
                                        // status as a default case in the main schema.
                                        // Instead we have this additional deserialization attempt fallback.
                                        let data = ::rmp_serde::from_slice::<UnknownStatus>(buf)?;

                                        match data.status.as_str() {
                                            #(#known_rep_statuses => Err(err),)*
                                            _ => Ok(Self::UnknownStatus {
                                                unknown_status: data.status,
                                                reason: data.reason,
                                            })
                                        }
                                    })
                            }
                        }
                    }

                }
            }
        }
    }
}

fn quote_custom_enum(name: &str, variants: &[GenCmdNestedTypeVariant]) -> TokenStream {
    let name = format_ident!("{}", name);
    let variants: Vec<TokenStream> = variants
        .iter()
        .map(|variant| {
            let variant_name = format_ident!("{}", variant.name);
            let variant_fields = quote_cmd_fields(variant.fields.as_ref(), false);
            quote! {
                #variant_name {
                    #(#variant_fields),*
                }
            }
        })
        .collect();
    quote! {
        #[::serde_with::serde_as]
        #[derive(Debug, Clone, ::serde::Deserialize, ::serde::Serialize, PartialEq, Eq)]
        pub enum #name {
            #(#variants),*
        }
    }
}

fn quote_custom_struct(name: &str, fields: &[GenCmdField]) -> TokenStream {
    let name = format_ident!("{}", name);
    let fields = quote_cmd_fields(fields, true);
    quote! {
        #[::serde_with::serde_as]
        #[derive(Debug, Clone, ::serde::Deserialize, ::serde::Serialize, PartialEq, Eq)]
        pub struct #name {
            #(#fields),*
        }
    }
}

fn quote_cmd_nested_types(nested_types: &[GenCmdNestedType]) -> Vec<TokenStream> {
    nested_types
        .iter()
        .map(|nested_type| match nested_type {
            GenCmdNestedType::Enum { name, variants } => quote_custom_enum(name, variants.as_ref()),
            GenCmdNestedType::Struct { name, fields } => quote_custom_struct(name, fields.as_ref()),
        })
        .collect()
}

fn quote_cmd_req_struct(req: &GenCmdReq) -> TokenStream {
    // TODO: handle unit field here !
    if req.other_fields.is_empty() {
        quote! {
            #[derive(Debug, Clone, ::serde::Serialize, ::serde::Deserialize, PartialEq, Eq)]
            pub struct Req;
        }
    } else {
        let fields = quote_cmd_fields(&req.other_fields, true);
        quote! {
            #[::serde_with::serde_as]
            #[derive(Debug, Clone, ::serde::Serialize, ::serde::Deserialize, PartialEq, Eq)]
            pub struct Req {
                #(#fields),*
            }
        }
    }
}

fn quote_cmd_rep_variant(rep: &GenCmdRep) -> TokenStream {
    let variant_name = format_ident!("{}", &snake_to_camel_case(&rep.status));
    let status_name = &rep.status;
    // TODO: handle unit field here !
    if rep.other_fields.is_empty() {
        quote! {
            #[serde(rename = #status_name)]
            #variant_name
        }
    } else {
        let fields = quote_cmd_fields(&rep.other_fields, false);
        quote! {
            #[serde(rename = #status_name)]
            #variant_name {
                #(#fields),*
            }
        }
    }
}

fn quote_cmd_rep_variants(reps: &[GenCmdRep]) -> Vec<TokenStream> {
    let mut variants: Vec<TokenStream> = reps
        .iter()
        .sorted_by(|a, b| a.status.cmp(&b.status))
        .map(quote_cmd_rep_variant)
        .collect();

    // `UnknownStatus` covers the case the server returns a valid message but with an unknown
    // status value (given change in error status only cause a minor bump in API version)
    // Note it is meaningless to serialize a `UnknownStatus` (you created the object from
    // scratch, you know what it is for baka !)
    variants.push(quote! {
        #[serde(skip)]
        UnknownStatus {
            unknown_status: String,
            reason: Option<String>
        }
    });
    variants
}

fn quote_cmd_fields(fields: &[GenCmdField], with_pub: bool) -> Vec<TokenStream> {
    fields
        .iter()
        // The fields will be sorted by their name in binary mode.
        .sorted_by(|a, b| a.name.cmp(&b.name))
        .map(|x| quote_cmd_field(x, with_pub))
        .collect()
}

fn quote_cmd_field(field: &GenCmdField, with_pub: bool) -> TokenStream {
    let mut attrs = Vec::<TokenStream>::new();

    let ty: syn::Type = syn::parse_str({
        let allowed_extra_types = field.allowed_extra_types.borrow();
        let ty = inspect_type(&field.ty, &*allowed_extra_types);
        if field.added_in_minor_revision {
            attrs.push(quote! {
                #[serde(default, skip_serializing_if = "::libparsec_types::Maybe::is_absent")]
            });
            format!("::libparsec_types::Maybe<{}>", ty)
        } else {
            ty
        }
        .as_ref()
    })
    .unwrap_or_else(|err| {
        panic!("Invalid type {:?} ({:?})", &field.ty, err);
    });

    let can_only_be_null = field.ty.starts_with("RequiredOption<");
    // let can_be_missing_or_null = field.ty.starts_with("NonRequiredOption<");
    // TODO: rework quote_serde_as to avoid syn ?
    attrs.push(quote_serde_as(&ty, can_only_be_null));

    let name = match field.name.as_ref() {
        "type" => {
            attrs.push(quote! {#[serde(rename = "type")]});
            format_ident!("ty")
        }
        name => format_ident!("{}", name),
    };

    if with_pub {
        quote! {
            #(#attrs)*
            pub #name: #ty
        }
    } else {
        quote! {
            #(#attrs)*
            #name: #ty
        }
    }
}
