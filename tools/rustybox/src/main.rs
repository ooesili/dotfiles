use anyhow::{Context, Result, anyhow};
use rustybox::cmd::lookup_subcommand;
use std::{env, path::Path};

fn main() -> Result<()> {
    let mut args = env::args();
    let argv0 = args.next().unwrap();
    let path = AsRef::<Path>::as_ref(&argv0)
        .file_name()
        .context("reading filename from argv[0]")?;

    let subcmd = env::var("RUSTYBOX_CMD").unwrap_or_else(|_| path.to_str().unwrap().to_owned());

    let main_fn = lookup_subcommand(&subcmd).ok_or(anyhow!("unknown argv[0]: {}", argv0))?;
    main_fn(args)
}
