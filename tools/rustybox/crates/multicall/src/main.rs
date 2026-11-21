use std::{collections::HashMap, env, path::Path};

use anyhow::{Context, Result};

pub type Command = fn() -> Result<()>;

fn main() -> Result<()> {
    let mut args = env::args();
    let argv0 = args.next().unwrap();
    let path = AsRef::<Path>::as_ref(&argv0)
        .file_name()
        .context("reading filename from argv[0]")?;

    let subcmd = env::var("RUSTYBOX_CMD").unwrap_or_else(|_| path.to_str().unwrap().to_owned());

    let main_fn =
        lookup_subcommand(&subcmd).with_context(|| format!("unknown argv[0]: {}", argv0))?;
    main_fn()
}

macro_rules! commands {
    ( $($name:ident),* , ) => {
        pub fn lookup_subcommand(subcmd: &str) -> Option<Command> {
            let mut cmds = HashMap::<String, Command>::new();
            $(
                cmds.insert(stringify!($name).replace("_", "-"), $name::main);
            )*
            cmds.get(subcmd).copied()
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/cmds_gen.rs"));
