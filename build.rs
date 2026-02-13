use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

fn sanitize_ident(s: &str) -> String {
    let mut out = String::new();
    for c in s.chars() {
        if c.is_ascii_alphanumeric() {
            out.push(c);
        } else {
            out.push('_');
        }
    }
    out
}

fn write_tests_for_files(files: &Vec<(String, String)>, prefix: &str) -> String {
    let mut out = String::new();
    for (file_name, stem) in files {
        let ident = sanitize_ident(stem);
        let fn_name = format!("test_{}_{}", prefix, ident);
        out.push_str(&format!(
            "#[test]\nfn {}() {{\n    let path = std::path::Path::new(\"tests/src/{}\");\n    let dir = std::path::Path::new(\"tests/{}\");\n    assert_file_path_eq(path, dir);\n}}\n\n",
            fn_name, file_name, prefix
        ));
    }
    out
}

fn main() -> std::io::Result<()> {
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not defined");
    let out_path = Path::new(&out_dir);

    // Ensure rebuild when tests change
    println!("cargo:rerun-if-changed=tests/src");
    println!("cargo:rerun-if-changed=parser_unit_tests");

    let test_src = Path::new("tests/src");
    let mut files: Vec<(String, String)> = Vec::new();

    if test_src.exists() {
        for entry in fs::read_dir(test_src)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() {
                if let Some(ext) = path.extension().and_then(|s| s.to_str()) {
                    if ext == "manta" {
                        if let (Some(fname), Some(stem)) = (
                            path.file_name().and_then(|s| s.to_str()),
                            path.file_stem().and_then(|s| s.to_str()),
                        ) {
                            files.push((fname.to_string(), stem.to_string()));
                        }
                    }
                }
            }
        }
    }

    // Generate parser tests
    let parser_tests = write_tests_for_files(&files, "parser");
    let noder_tests = write_tests_for_files(&files, "noder");
    let lexer_tests = write_tests_for_files(&files, "lexer");

    let mut p = File::create(out_path.join("generated_parser_tests.rs"))?;
    p.write_all(parser_tests.as_bytes())?;

    let mut n = File::create(out_path.join("generated_noder_tests.rs"))?;
    n.write_all(noder_tests.as_bytes())?;

    let mut l = File::create(out_path.join("generated_lexer_tests.rs"))?;
    l.write_all(lexer_tests.as_bytes())?;

    // parser unit tests (in parser_unit_tests directory)
    let unit_src = Path::new("parser_unit_tests");
    let mut unit_files: Vec<(String, String)> = Vec::new();

    if unit_src.exists() {
        for entry in fs::read_dir(unit_src)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() {
                if let Some(ext) = path.extension().and_then(|s| s.to_str()) {
                    if ext == "case" {
                        if let (Some(fname), Some(stem)) = (
                            path.file_name().and_then(|s| s.to_str()),
                            path.file_stem().and_then(|s| s.to_str()),
                        ) {
                            unit_files.push((fname.to_string(), stem.to_string()));
                        }
                    }
                }
            }
        }
    }

    let mut u = File::create(out_path.join("generated_parser_unit_tests.rs"))?;
    {
        let mut out = String::new();
        for (file_name, stem) in &unit_files {
            let ident = sanitize_ident(stem);
            let fn_name = format!("test_parser_unit_{}", ident);
            out.push_str(&format!(
                "#[test]\nfn {}() {{\n    let path = std::path::Path::new(\"parser_unit_tests/{}\");\n    assert_case_file_eq(path);\n}}\n\n",
                fn_name, file_name
            ));
        }
        u.write_all(out.as_bytes())?;
    }

    Ok(())
}
