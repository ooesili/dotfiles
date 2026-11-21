use std::{
    env::{self},
    fs::{self, File},
    io::Write,
    path::PathBuf,
};

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let manifest_path = manifest_dir.join("Cargo.toml");
    println!(
        "cargo::rerun-if-changed={}",
        manifest_path.to_string_lossy()
    );

    let text = fs::read_to_string(&manifest_path).unwrap();
    let manifest: toml::Table = text.parse().unwrap();

    let mut programs = vec![];
    if let Some(deps) = manifest.get("dependencies").and_then(|d| d.as_table()) {
        for (crate_name, spec) in deps {
            if spec.get("path").is_none() {
                continue;
            };
            programs.push(crate_name);
        }
    }

    programs.sort();

    let mut cmds_gen_rs = File::create(out_dir.join("cmds_gen.rs")).unwrap();
    writeln!(cmds_gen_rs, "commands!(").unwrap();
    for program in programs {
        writeln!(cmds_gen_rs, "{},", program).unwrap();
    }
    writeln!(cmds_gen_rs, ");").unwrap();
}
