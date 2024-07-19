pub mod clipboard;

use self::clipboard::{get_current_clipboard, Clipboard};

use super::rclipd::{Action, Header, Selection};
use anyhow::{bail, Context, Result};
use std::{
    env,
    io::{self, Write},
    net::TcpStream,
};

pub fn main(mut args: env::Args) -> Result<()> {
    let clipboard = get_current_clipboard();

    match args.next() {
        Some(command) => {
            let selection = parse_selection_flag(args.next())?;
            match command.as_str() {
                "copy" => cmd_copy(clipboard, selection),
                "paste" => cmd_paste(clipboard, selection),
                _ => bail!("usage: command must be one of 'copy' or 'paste'"),
            }
        }
        None => bail!("usage: no command given, command must be one of 'copy' or 'paste'"),
    }
}

fn parse_selection_flag(flag: Option<String>) -> Result<Selection> {
    Ok(match flag {
        Some(flag) => match flag.as_str() {
            "--primary" => Selection::Primary,
            "--clipboard" => Selection::Clipboard,
            _ => bail!("invalid selection flag: {}", flag),
        },
        None => Selection::Primary,
    })
}

fn cmd_paste(clipboard: Box<dyn Clipboard>, selection: Selection) -> Result<()> {
    match TcpStream::connect("127.0.0.1:9022") {
        Ok(mut conn) => {
            let header = Header {
                action: Action::Paste,
                selection,
            };
            let header_byte = [header.into()];
            conn.write_all(&header_byte)
                .context("writing header byte")?;
            let stdout = io::stdout();
            io::copy(&mut conn, &mut stdout.lock()).context("copying clipboard over network")?;
        }
        Err(_) => clipboard.paste(selection)?,
    }

    Ok(())
}

fn cmd_copy(clipboard: Box<dyn Clipboard>, selection: Selection) -> Result<()> {
    match TcpStream::connect("127.0.0.1:9022") {
        Ok(mut conn) => {
            let header = Header {
                action: Action::Copy,
                selection,
            };
            let header_byte = [header.into()];
            conn.write_all(&header_byte)
                .context("writing action byte")?;
            let stdin = io::stdin();
            io::copy(&mut stdin.lock(), &mut conn).context("copying clipboard over network")?;
        }
        Err(_) => clipboard.copy(selection)?,
    }

    Ok(())
}
