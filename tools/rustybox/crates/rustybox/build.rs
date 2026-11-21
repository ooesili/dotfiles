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
        for (prog_name, spec) in deps {
            if spec.get("path").is_none() {
                continue;
            };
            let mod_name = prog_name.replace("-", "_");
            programs.push((prog_name, mod_name));
        }
    }

    let mut cmds_gen_rs = File::create(out_dir.join("cmds_gen.rs")).unwrap();
    writeln!(cmds_gen_rs, "commands!(").unwrap();
    for (prog_name, mod_name) in programs {
        writeln!(cmds_gen_rs, "({:?},{}),", prog_name, mod_name).unwrap();
    }
    writeln!(cmds_gen_rs, ");").unwrap();
}
