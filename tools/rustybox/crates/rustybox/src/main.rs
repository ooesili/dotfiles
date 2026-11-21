use std::{env, path::Path};

use anyhow::{Context, Result, anyhow};

fn main() -> Result<()> {
    let mut args = env::args();
    let argv0 = args.next().unwrap();
    let path = AsRef::<Path>::as_ref(&argv0)
        .file_name()
        .context("reading filename from argv[0]")?;

    let subcmd = env::var("RUSTYBOX_CMD").unwrap_or_else(|_| path.to_str().unwrap().to_owned());
    dispatch_subcmd(&subcmd)
}

macro_rules! commands {
    ( $(($prog_name:literal, $mod_name:ident)),* , ) => {
        pub fn dispatch_subcmd(subcmd: &str) -> Result<()> {
            match subcmd {
                $(
                    $prog_name => $mod_name::main(),
                )*
                _ => Err(anyhow!("unknown argv[0]: {}", subcmd)),
            }
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/cmds_gen.rs"));
