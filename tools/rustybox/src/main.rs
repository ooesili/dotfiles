use anyhow::{anyhow, Context, Result};
use std::{env, path::Path};

fn main() -> Result<()> {
    let mut args = env::args();
    let argv0 = args.next().unwrap();
    let path = AsRef::<Path>::as_ref(&argv0)
        .file_name()
        .context("reading filename from argv[0]")?;

    let subcmd = env::var("RUSTYBOX_CMD").unwrap_or(path.to_str().unwrap().to_owned());

    match subcmd.as_str() {
        "batteryd" => rustybox::batteryd::main(args),
        "configctl" => rustybox::configctl::main(args),
        "mediactl" => rustybox::mediactl::main(args),
        "rclip" => rustybox::rclip::main(args),
        "rclipd" => rustybox::rclipd::main(args),
        _ => Err(anyhow!("unknown argv[0]: {}", argv0)),
    }
}
