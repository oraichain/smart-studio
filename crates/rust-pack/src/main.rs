#![allow(unused)]
use core::panic;
use std::fs;
use std::fs::read_to_string;
use std::path::Path;
use std::process::Command;

use regex::Regex;

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
        let c = input.get(end_ind..=end_ind).unwrap_or_default();
        if c.eq(start_char) {
            matched += 1;
        } else if c.eq(end_char) {
            matched -= 1;
        }

        if matched == 0 {
            break;
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
                if input.get(look_back..look_back + 3).unwrap_or_default() == "///" {
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
        .map(|mat| (mat.end() + 1, mat.end()))
        .collect();
    remove_from_indices(input, indices)
}
fn remove_from_indices(input: &str, indices: Vec<(usize, usize)>) -> String {
    let mut peak_start_ind: usize = 0;
    let mut output = String::new();
    for (start_ind, search_ind) in indices {
        let end_ind = peak_until(&input, search_ind, "{", "}");
        // sub group
        if end_ind < peak_start_ind {
            continue;
        }
        // println!("matched: {}", input.get(start_ind..end_ind).unwrap());
        output.push_str(input.get(peak_start_ind..start_ind).unwrap_or_default());
        peak_start_ind = end_ind;
    }

    output.push_str(input.get(peak_start_ind..input.len()).unwrap_or_default());

    output
}

fn remove_function_body(input: &str) -> String {
    lazy_static::lazy_static! {
        static ref FN_REGEX: Regex = Regex::new(r"[\r\t\n\s]+fn\s+[^{]+\{").unwrap();
    }

    remove_from_reg(input, &FN_REGEX)
}

fn remove_test_mod(input: &str) -> String {
    lazy_static::lazy_static! {
        static ref TEST_MOD_REGEX: Regex = Regex::new(r"[\r\t\n\s]+#\[cfg\(test\)\][^{]+\{").unwrap();
    }

    remove_from_reg(input, &TEST_MOD_REGEX)
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
            explicit_path: current_mod_state.path_seen,
            pub_prefix: "",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub mod ") {
        Some(Mod {
            explicit_path: current_mod_state.path_seen,
            pub_prefix: "pub ",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub(crate) mod ") {
        Some(Mod {
            explicit_path: current_mod_state.path_seen,
            pub_prefix: "pub(crate) ",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub(self) mod ") {
        Some(Mod {
            explicit_path: current_mod_state.path_seen,
            pub_prefix: "pub(self) ",
            name: clean_token(line),
        })
    } else if let Some(line) = line.strip_prefix("pub(super) mod ") {
        Some(Mod {
            explicit_path: current_mod_state.path_seen,
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
                // println!("explicit path found: {:?}", ep);
                path.parent().unwrap().join(ep)
            } else if same_level_path.exists() {
                same_level_path
            } else if folder_path.exists() {
                folder_path
            } else {
                println!(
                    "same_level_path: {:?}\nfolder_path: {:?}\n",
                    same_level_path, folder_path
                );
                return Err(MyError {
                    libstack: vec![path.to_string_lossy().to_string()],
                    cause_module: folder_path.to_string_lossy().to_string(),
                });
            };
            if let Err(mut e) = put_module_in_string(output, &child_path, depth + 1, rr) {
                e.libstack.push(path.to_string_lossy().to_string());
                return Err(e);
            }
            output.push_line("}");
        } else {
            output.push_line(line);
        }
    }
    Ok(())
}

fn main() {
    let rustc_result = Command::new("rustc")
        .args(&["--print", "sysroot"])
        .output()
        .expect("Failed to execute rustc")
        .stdout;
    let sysroot_path = format!(
        "{}/lib/rustlib/src/rust/library",
        std::str::from_utf8(&rustc_result).expect("rustc output wasn't utf8").trim()
    );
    let cosmwasm_path = std::env::var("COSMWASM_PATH").unwrap();
    let output_path = std::env::var("OUTPUT_PATH")
        .map(|str| str.trim_end_matches('/').to_string())
        .unwrap_or("src/rust".to_string());

    // rust library
    let lib_rust_paths = [
        (sysroot_path.clone(), vec!["std", "alloc", "core"], format!("{}/", output_path)),
        (
            format!("{}/packages", cosmwasm_path),
            vec!["std", "derive", "schema", "schema-derive", "crypto", "storage"],
            format!("{}/cosmwasm-", output_path),
        ),
    ];

    for (rust_path, packages, out_prefix) in lib_rust_paths {
        for package in packages {
            let path_string = &format!("{}/{}/src/lib.rs", rust_path, package);
            let path = Path::new(path_string);

            let output_path = format!("{}{}.rs", out_prefix, package);

            let mut output = String::default();
            put_module_in_string(&mut output, path, 0, 4000).unwrap();
            // remove test mod
            output = remove_test_mod(&output);
            // remove function body
            output = remove_function_body(&output);
            fs::write(output_path, output.clone()).unwrap();
        }
    }
}
