use anyhow::{Context, Result, anyhow};
use rustybox::cmd::lookup_subcommand;
use std::{env, ffi::OsStr, path::Path};

fn main() -> Result<()> {
    let mut args = env::args();
    let argv0 = args.next().unwrap();

    let subcmd = env::var("RUSTYBOX_CMD")
        .unwrap_or_else(|_| get_filename(path).to_str().unwrap().to_owned());

    let main_fn = lookup_subcommand(&subcmd).ok_or(anyhow!("unknown argv[0]: {}", argv0))?;
    main_fn(args)
}

fn get_filename(argv0: &str) -> Result<&OsStr> {
    Path::new(argv0)
        .file_name()
        .context("reading filename from argv[0]")
}
