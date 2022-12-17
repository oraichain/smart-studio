#![allow(unused)]
use change_json::ChangeJson;
use clap::{App, Arg};
use core::panic;
use regex::{Regex, RegexBuilder};
use rust_pack::extractor;
use std::collections::HashMap;
use std::fs;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};
use std::process::Command;

struct Mod<'a> {
    pub_prefix: &'a str,
    explicit_path: Option<&'a str>,
    name: &'a str,
}

#[derive(Default, Debug)]
struct ModState<'a> {
    path_seen: Option<&'a str>,
    in_attribute: bool,
}

fn remove_comment(line: &str) -> &str {
    if let Some((l, _)) = line.split_once("//") {
        l.trim()
    } else {
        line
    }
}

fn clean_token(line: &str) -> &str {
    if let Some(l) = line.strip_prefix("r#") {
        l
    } else {
        line
    }
}

fn peak_until(input: &str, start: usize, start_char: &str, end_char: &str) -> usize {
    let mut matched = 1;
    let mut end_ind = start + 1;

    while end_ind < input.len() {
        let prev_c = input.get(end_ind - 1..=end_ind - 1).unwrap_or_default();
        // quote '{' or '}', just continue
        if prev_c != "'" {
            let c = input.get(end_ind..=end_ind).unwrap_or_default();
            if c.eq(start_char) {
                matched += 1;
            } else if c.eq(end_char) {
                matched -= 1;
            }

            if matched == 0 {
                break;
            }
        }
        end_ind += 1;
    }
    end_ind
}

fn remove_from_reg(input: &str, regex: &Regex) -> String {
    lazy_static::lazy_static! {
        static ref END_STATEMENT_REGEX: Regex = Regex::new(r";[\s\t]*\n").unwrap();
    }

    let indices: Vec<(usize, usize)> = regex
        .find_iter(input)
        .filter(|mat| {
            // ignore in comment line
            let mut look_back = mat.start();
            while input.get(look_back..=look_back).unwrap_or_default() != "\n" {
                if input.get(look_back..look_back + 2).unwrap_or_default() == "//" {
                    return false;
                }
                look_back -= 1;
            }

            // end statement, no processing
            if END_STATEMENT_REGEX.is_match(input.get(mat.start()..mat.end()).unwrap_or_default()) {
                return false;
            }

            return true;
        })
        .map(|mat| (mat.start(), mat.end()))
        .collect();
    remove_from_indices(input, indices)
}

fn remove_from_indices(input: &str, indices: Vec<(usize, usize)>) -> String {
    let mut peak_start_ind: usize = 0;
    let mut output = String::new();
    let mut count = 0;
    for (orig_start_ind, search_ind) in indices {
        let start_ind = search_ind + 1;
        let end_ind = peak_until(&input, search_ind, "{", "}");

        // sub group
        if end_ind < peak_start_ind {
            continue;
        }
        // println!("matched: {}", input.get(start_ind..end_ind).unwrap());

        output.push_str(input.get(peak_start_ind..start_ind).unwrap_or_default());
        peak_start_ind = end_ind;

        count += 1;
    }

    output.push_str(input.get(peak_start_ind..input.len()).unwrap_or_default());

    output
}

fn remove_function_body(input: &str) -> String {
    lazy_static::lazy_static! {
        static ref FN_REGEX: Regex = RegexBuilder::new(r#"[\r\t\n\s]+fn\s+[^{]+\{"#).case_insensitive(true).build().unwrap();
    }
    remove_from_reg(input, &FN_REGEX)
}

fn remove_test_mod(input: &str) -> String {
    lazy_static::lazy_static! {
        static ref TEST_MOD_REGEX: Regex = RegexBuilder::new(r#"[\r\t\n\s]+#\[cfg\(test\)\][^{]+\{"#).case_insensitive(true).build().unwrap();
    }
    remove_from_reg(input, &TEST_MOD_REGEX)
}

fn remove_extern_c(input: &str) -> String {
    lazy_static::lazy_static! {
        static ref EXTERN_C_REGEX: Regex = RegexBuilder::new(r#"[\r\t\n\s]+extern\s+"C"[^{]+\{"#).case_insensitive(true).build().unwrap();
    }
    remove_from_reg(input, &EXTERN_C_REGEX)
}

fn remove_unstable_feature(input: &str) -> String {
    lazy_static::lazy_static! {
        static ref UNSTABLE_FEATURE_REGEX: Regex = RegexBuilder::new(r#"[\r\t\n\s]+#\[unstable\(feature\s+=[^{]+\{"#).case_insensitive(true).build().unwrap();
    }
    remove_from_reg(input, &UNSTABLE_FEATURE_REGEX)
}

fn remove_skip_format(input: &str) -> String {
    lazy_static::lazy_static! {
        static ref SKIP_FORMAT_REGEX: Regex = RegexBuilder::new(r#"[\r\t\n\s]+#\[rustfmt::skip\][^{]+\{"#).case_insensitive(true).build().unwrap();
    }
    remove_from_reg(input, &SKIP_FORMAT_REGEX)
}

fn extract_path_seen(path_seen: Option<&str>) -> Option<&str> {
    path_seen.map(|seen| {
        if let Some(ind) = seen.find("\"") {
            return seen.get(..ind).unwrap_or_default();
        }
        seen
    })
}

fn is_external_mod<'a>(mod_state: &mut ModState<'a>, line: &'a str) -> Option<Mod<'a>> {
    let line = remove_comment(line);
    if line.is_empty() {
        return None;
    }
    if line.starts_with("#[path = ") {
        mod_state.path_seen = Some(&line[10..line.len() - 2]);
        return None;
    }
    if line.starts_with("#[") {
        if !line.ends_with(']') {
            mod_state.in_attribute = true;
        }
        return None;
    }
    if mod_state.in_attribute {
        if line.ends_with(']') {
            mod_state.in_attribute = false;
        }
        return None;
    }
    let current_mod_state = std::mem::take(mod_state);

    if !line.ends_with(';') {
        return None;
    }

    let line = &line[..line.len() - 1];
    if let Some(line) = line.strip_prefix("mod ") {
        Some(Mod {
            explicit_path: extract_path_seen(current_mod_state.path_seen),
            pub_prefix: "",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub mod ") {
        Some(Mod {
            explicit_path: extract_path_seen(current_mod_state.path_seen),
            pub_prefix: "pub ",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub(crate) mod ") {
        Some(Mod {
            explicit_path: extract_path_seen(current_mod_state.path_seen),
            pub_prefix: "pub(crate) ",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub(self) mod ") {
        Some(Mod {
            explicit_path: extract_path_seen(current_mod_state.path_seen),
            pub_prefix: "pub(self) ",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub(super) mod ") {
        Some(Mod {
            explicit_path: extract_path_seen(current_mod_state.path_seen),
            pub_prefix: "pub(super) ",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub(in ") {
        panic!("pub in not supported: {}", line);
    } else {
        None
    }
}

trait MyStringMethods {
    fn push_line(&mut self, line: &str);
}

impl MyStringMethods for String {
    fn push_line(&mut self, line: &str) {
        self.push_str(line);
        self.push('\n');
    }
}

#[derive(Default, Debug)]
struct MyError {
    libstack: Vec<String>,
    cause_module: String,
}

fn put_module_in_string(
    output: &mut String,
    path: &Path,
    depth: usize,
    mut expand_cnt: i32,
) -> Result<(), MyError> {
    let src = read_to_string(path).map_err(|_x| MyError {
        libstack: vec![],
        cause_module: path.to_string_lossy().to_string(),
    })?;
    let mut mod_state = ModState::default();
    for line in src.lines() {
        if let Some(m) = is_external_mod(&mut mod_state, line) {
            if expand_cnt == 0 {
                continue;
            };
            let rr = 10000;
            expand_cnt -= 1;
            // println!("{} mod found: {}", ">".repeat(depth), line);
            output.push_line(&format!("{}mod {} {{", m.pub_prefix, m.name));
            let mut parent_path = path.parent().unwrap().to_owned();
            let file_name =
                path.file_name().unwrap().to_str().unwrap().strip_suffix(".rs").unwrap();
            if file_name != "lib" && file_name != "mod" {
                parent_path = parent_path.join(file_name);
            }
            let same_level_path = parent_path.join(format!("{}.rs", m.name));
            let folder_path = parent_path.join(format!("{}/mod.rs", m.name));
            let child_path = if let Some(ep) = m.explicit_path {
                path.parent().map(|p| p.join(ep))
            } else if same_level_path.exists() {
                Some(same_level_path)
            } else if folder_path.exists() {
                Some(folder_path)
            } else {
                // ignore to continue
                None
            };
            if let Some(child_path) = child_path {
                if let Err(mut e) = put_module_in_string(output, &child_path, depth + 1, rr) {
                    e.libstack.push(path.to_string_lossy().to_string());
                    return Err(e);
                }
            }
            output.push_line("}");
        } else {
            output.push_line(line);
        }
    }
    Ok(())
}

fn main() {
    let matches = App::new("Rust library packing")
        .version("0.1.0")
        .long_about("Run a rust-pack toolchain (cargo run -p rust-pack --release -- cosmwasm).")
        .author("Thanh Tu <tu@orai.io>")
        .arg(
            Arg::with_name("COSMWASM_PATH")
                .help("cosmwasm path to read and pack")
                .default_value("../cosmwasm")
                .required(true)
                .index(1),
        )
        .arg(Arg::from_usage("--toolchain=[TOOLCHAIN|RUST_PATH] 'rustup toolchain'"))
        .arg(Arg::from_usage("--output=[rust|json] 'pack type'"))
        .get_matches();

    let toolchain = matches.value_of("toolchain").unwrap_or("+stable");
    let sysroot_path = &match toolchain.starts_with("+") {
        false => toolchain.trim_end_matches("/").to_string(),
        true => {
            let rustc_result = Command::new("rustc")
                .args(&[toolchain, "--print", "sysroot"])
                .output()
                .expect("Failed to execute rustc")
                .stdout;
            format!(
                "{}/lib/rustlib/src/rust/library",
                std::str::from_utf8(&rustc_result).expect("rustc output wasn't utf8").trim()
            )
        }
    };
    let cosmwasm_path =
        &format!("{}/packages", matches.value_of("COSMWASM_PATH").unwrap().trim_end_matches("/"));
    let output_type = matches.value_of("output").unwrap_or_default();

    let cargo_path = &fs::read_dir(format!("{}/.cargo/registry/src", env!("HOME")))
        .unwrap()
        .find(|dir| dir.as_ref().unwrap().file_name().to_string_lossy().starts_with("github.com-"))
        .unwrap()
        .unwrap()
        .path()
        .to_string_lossy()
        .to_string();

    // rust library
    let lib_rust_paths = [
        if Path::new(&format!("{}/std", sysroot_path)).exists() {
            (sysroot_path, vec!["std", "alloc", "core"], "lib", "src/lib.rs")
        } else {
            (sysroot_path, vec!["libstd", "liballoc", "libcore"], "", "lib.rs")
        },
        (
            cosmwasm_path,
            vec!["std", "derive", "schema", "schema-derive", "crypto", "storage"],
            "cosmwasm-",
            "src/lib.rs",
        ),
        (
            cargo_path,
            vec!["thiserror-1.0.23", "thiserror-impl-1.0.23", "proc-macro2-1.0.6"],
            "",
            "src/lib.rs",
        ),
    ];

    let mut crate_map = HashMap::new();

    for (rust_path, packages, out_prefix, entry_src) in lib_rust_paths {
        for package in packages {
            let path_string = &format!("{}/{}/{}", rust_path, package, entry_src);
            let path = Path::new(path_string);

            let mut output = String::default();
            put_module_in_string(&mut output, path, 0, 4000).unwrap();

            let name = format!("{}{}", out_prefix, package);
            output = remove_skip_format(&output);
            output = remove_extern_c(&output);
            if name != "libcore" {
                output = remove_unstable_feature(&output);
            } else {
                let (left, right) = output.split_at(output.len() * 24 / 100); // 23-24%
                let mut keep_unwrap_flattern = String::from(left);
                keep_unwrap_flattern.push_str(&remove_unstable_feature(right));
                output = keep_unwrap_flattern;
            }
            output = remove_test_mod(&output);
            output = remove_function_body(&output);

            // fix reference for libcore
            if name.eq("libcore") {
                output = output.replace(
                    "[the reference]: ../../../",
                    "[the reference]: https://doc.rust-lang.org/nightly/",
                );
            }

            if name.starts_with("thiserror-") {
                output = output
                    .replace(
                    "https://img.shields.io/badge/github-8da0cb?style=for-the-badge&labelColor=555555&logo=github",
                    "https://studio.orai.network/assets/img/github.svg")
                    .replace(
                        "https://img.shields.io/badge/crates.io-fc8d62?style=for-the-badge&labelColor=555555&logo=rust",
                    "https://studio.orai.network/assets/img/crates-io.svg")
                    .replace(
                        "https://img.shields.io/badge/docs.rs-66c2a5?style=for-the-badge&labelColor=555555&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K",
                     "https://studio.orai.network/assets/img/docs-rs.svg");
            }

            if output_type.is_empty() || output_type.eq("json") {
                // write change json
                crate_map.insert(name.clone(), output.clone());
            }

            if output_type.is_empty() || output_type.eq("rust") {
                fs::write(format!("src/rust/{}.rs", name), output).unwrap();
            }
        }
    }

    if output_type.is_empty() || output_type.eq("json") {
        let change = extractor::load_change_from_files(
            crate_map.get("libstd").unwrap().clone(),
            crate_map.get("libcore").unwrap().clone(),
            crate_map.get("liballoc").unwrap().clone(),
            crate_map.get("cosmwasm-derive").unwrap().clone(),
            crate_map.get("cosmwasm-schema-derive").unwrap().clone(),
            crate_map.get("cosmwasm-schema").unwrap().clone(),
            crate_map.get("cosmwasm-std").unwrap().clone(),
            crate_map.get("cosmwasm-crypto").unwrap().clone(),
            crate_map.get("cosmwasm-storage").unwrap().clone(),
            crate_map.get("thiserror-1.0.23").unwrap().clone(),
            crate_map.get("thiserror-impl-1.0.23").unwrap().clone(),
            crate_map.get("proc-macro2-1.0.6").unwrap().clone(),
        );

        let json = ChangeJson::from(&change);
        let text = serde_json::to_string(&json).unwrap_or_else(|err| {
            panic!("Error while parsing ChangeJson object to string: {}", err)
        });
        fs::write("change.json", text).unwrap();
    }
}

// fn main() {
//     let mut output = fs::read_to_string("/Users/phamtu/Projects/smart-studio/test.rs").unwrap();

//     output = remove_unstable_feature(&output);
//     println!("{}", output);
// }
