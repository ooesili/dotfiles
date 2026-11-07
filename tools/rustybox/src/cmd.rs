use anyhow::Result;
use std::collections::HashMap;
use std::env;

pub type Command = fn(env::Args) -> Result<()>;

macro_rules! commands {
    ( $(($name:ident, $out:literal, $source_path:literal)),* , ) => {
        $(
            #[path = $source_path]
            pub mod $name;
        )*

        pub fn lookup_subcommand(subcmd: &str) -> Option<Command> {
            let mut cmds = HashMap::<String, Command>::new();
            $(
                cmds.insert(stringify!($name).replace("_", "-"), crate::cmd::$name::main);
            )*
            cmds.get(subcmd).copied()
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/cmds_gen.rs"));
