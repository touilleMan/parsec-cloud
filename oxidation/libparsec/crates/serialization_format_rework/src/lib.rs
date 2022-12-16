// Parsec Cloud (https://parsec.cloud) Copyright (c) BUSL-1.1 (eventually AGPL-3.0) 2016-present Scille SAS

mod old_parser;
// pub(crate) mod protocol;

use crate::old_parser::{GenCmdsFamilly, JsonCmd};

use proc_macro::TokenStream;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};
use syn::{parse_macro_input, LitStr};

// use protocol::{intermediate, parser};

// /// Procedural macro that take a directory or a file path.
// ///
// /// If the `path` is a directory it **MUST** only contains json files.
// ///
// /// If the `path` is a file it **MUST** be a json file.
// #[proc_macro]
// pub fn parsec_protocol(path: TokenStream) -> TokenStream {
//     let pathname = parse_macro_input!(path as LitStr).value();
//     let path = path_from_str(&pathname);
//     let familly = path
//         .file_name()
//         .and_then(|os_str| os_str.to_str())
//         .expect("Invalid filename");
//     let collection = if path.is_dir() {
//         let content = content_from_dir(&path).expect("Failed to get content from directory");
//         parser::ProtocolCollection::with_protocols(familly, content)
//     } else {
//         let content = content_from_file(&path).expect("Failed to get content from file");
//         parser::ProtocolCollection::with_protocol(familly, content)
//     };
//     let collection = intermediate::ProtocolCollection::from(collection);
//     collection.quote().into_token_stream().into()
// }

fn path_from_str(path: &str) -> PathBuf {
    let manifest_dir_path: PathBuf = std::env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR must be set")
        .parse()
        .expect("CARGO_MANIFEST_DIR must be a valid path");
    manifest_dir_path.join(path)
}

fn content_from_file(path: &PathBuf) -> String {
    let file = File::open(path).expect("Cannot open the json file");
    let buf = BufReader::new(file);
    let mut content = String::new();
    for (i, line) in buf.lines().enumerate() {
        let line = line.unwrap_or_else(|_| panic!("Non-Utf-8 character found in line {i}"));
        let line = match line.split_once("//") {
            Some((line, _)) => line,
            None => &line,
        };
        content.push_str(line)
    }
    content
}

// pub(crate) fn content_from_dir(path: &PathBuf) -> anyhow::Result<Vec<parser::Protocol>> {
//     let dir = std::fs::read_dir(path)
//         .map_err(anyhow::Error::new)
//         .context(format!("Reading directory `{}`", path.to_string_lossy()))?;
//     dir.filter_map(|entry| entry.ok())
//         .map(|entry| content_from_file(&entry.path()))
//         .collect()
// }

// pub(crate) fn content_from_file(path: &PathBuf) -> anyhow::Result<parser::Protocol> {
//     let filename = path.to_string_lossy();
//     let file = File::open(path)
//         .map_err(anyhow::Error::new)
//         .context("Opening file")?;
//     let buf = BufReader::new(file);
//     let mut content = String::new();
//     for (i, line) in buf.lines().enumerate() {
//         let line =
//             line.unwrap_or_else(|_| panic!("{}:{} Non UTF-8 characters found", &filename, i));
//         let line = line
//             .split_once("//")
//             .map(|(before, _after)| before)
//             .unwrap_or(&line);
//         content.push_str(line);
//         content.push('\n');
//     }
//     content_from_str(&content, &filename)
// }

// pub(crate) fn content_from_str(
//     content: &str,
//     origin: &str,
// ) -> Result<parser::Protocol, anyhow::Error> {
//     serde_json::from_str(content)
//         .map_err(anyhow::Error::new)
//         .context(format!("current file `{origin}`"))
// }

/// Procedural macro that takes a directory containing one JSON file per protocol command.
#[proc_macro]
pub fn parsec_protocol_familly(path: TokenStream) -> TokenStream {
    let pathname = parse_macro_input!(path as LitStr).value();
    let path = path_from_str(&pathname);

    let familly_name = path
        .file_name()
        .and_then(|os_str| os_str.to_str())
        .expect("Invalid path, cannot determine protocol familly name")
        .to_owned();

    let dir = std::fs::read_dir(path).expect("Cannot read directory");

    let gen_cmds_familly = {
        let mut json_cmds = vec![];
        for json_path in dir.filter_map(|entry| entry.ok()) {
            let json_content = {
                let json_without_outer_struct = content_from_file(&json_path.path());
                // Hack around the fact Miniserde only supports struct as root ;-)
                format!("{{\"items\": {json_without_outer_struct} }}")
            };

            let json_cmd = match miniserde::json::from_str::<JsonCmd>(&json_content) {
                Ok(json_cmd) => json_cmd,
                Err(err) => {
                    panic!("{:?}: JSON spec is not valid ({err:?})", json_path.path());
                }
            };
            json_cmds.push(json_cmd);
        }
        GenCmdsFamilly::new(json_cmds, &familly_name)
    };

    TokenStream::from(gen_cmds_familly.quote())
    // TokenStream::from(gen_cmds_familly.versions[&1]["ping"].quote())
}

// Useful for tests to avoid having to deal with file system
#[proc_macro]
pub fn generate_protocol_familly_from_contents(json_contents: TokenStream) -> TokenStream {
    let json_contents: Vec<String> = {
        let content = parse_macro_input!(json_contents as LitStr).value();
        // Consider empty line as a separator between json files
        content
            .split("\n\n")
            .into_iter()
            .map(String::from)
            .collect()
    };
    let familly_name = "protocol";
    let mut json_cmds = vec![];
    for json_without_outer_struct in json_contents {
        // Hack around the fact Miniserde only supports struct as root ;-)
        let json_content = format!("{{\"items\": {json_without_outer_struct} }}");
        let json_cmd =
            miniserde::json::from_str::<JsonCmd>(&json_content).expect("JSON spec is not valid");
        json_cmds.push(json_cmd);
    }
    let gen_cmds_familly = GenCmdsFamilly::new(json_cmds, familly_name);
    TokenStream::from(gen_cmds_familly.quote())
}

#[proc_macro]
pub fn parsec_data(path: TokenStream) -> TokenStream {
    let path = parse_macro_input!(path as LitStr).value();
    let content = content_from_file(&path_from_str(&path));

    let data: old_parser::Data = miniserde::json::from_str(&content).expect("Data is not valid");
    TokenStream::from(data.quote())
}
