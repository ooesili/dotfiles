use anyhow::{anyhow, Context, Result};
use std::{env, path::Path};

fn main() -> Result<()> {
    let mut args = env::args();
    let argv0 = args.next().unwrap();
    let path = AsRef::<Path>::as_ref(&argv0)
        .file_name()
        .context("reading filename from argv[0]")?;

    let subcmd = env::var("RUSTYBOX_CMD").unwrap_or_else(|_| path.to_str().unwrap().to_owned());

    let main_fn = lookup_command(&subcmd).ok_or(anyhow!("unknown argv[0]: {}", argv0))?;
    main_fn(args)
}

type Command = fn(env::Args) -> Result<()>;

fn lookup_command(subcmd: &str) -> Option<Command> {
    Some(match subcmd {
        "batteryd" => rustybox::cmd::batteryd::main,
        "bluetoothd" => rustybox::cmd::bluetoothd::main,
        "configctl" => rustybox::cmd::configctl::main,
        "mediactl" => rustybox::cmd::mediactl::main,
        "rclip" => rustybox::cmd::rclip::main,
        "rclipd" => rustybox::cmd::rclipd::main,
        "sway-windows" => rustybox::cmd::sway_windows::main,
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn all_subcommands_registered() {
        let entries = fs::read_dir("src/cmd").unwrap();
        for entry in entries {
            let entry = entry.unwrap();
            if entry.path().extension().unwrap().to_str() == Some("rs") {
                let file_name = entry.file_name();
                let name = &file_name
                    .to_str()
                    .unwrap()
                    .strip_suffix(".rs")
                    .unwrap()
                    .replace("_", "-");
                assert!(
                    lookup_command(name).is_some(),
                    "command '{}' is not registered",
                    name
                );
            }
        }
    }
}
