use std::env;
use std::env::current_dir;
use std::fs;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let pwd = current_dir().unwrap();

    let cmds_file = File::open("commands.txt").unwrap();
    let reader = BufReader::new(cmds_file);

    let mut source = "commands!(".to_string();
    for (lineno, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        let Some((_, exec_name)) = line.rsplit_once("/") else {
            panic!("malformed line: commands.txt:{}", lineno);
        };
        let mod_name = exec_name.replace("-", "_");

        let source_path = pwd.join("src").join("cmd").join(format!("{mod_name}.rs"));
        source.push_str(&format!(
            "({},{:?},{:?}),",
            mod_name, exec_name, source_path
        ));
    }
    source.push_str(");");

    let cmds_gen_rs = Path::new(&out_dir).join("cmds_gen.rs");
    fs::write(cmds_gen_rs, source).unwrap();

    println!("cargo::rerun-if-changed=commands.txt");
}
