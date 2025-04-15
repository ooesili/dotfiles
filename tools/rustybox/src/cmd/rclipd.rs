use anyhow::{bail, Context, Result};
use std::{
    env,
    io::{self, Read},
    net::{TcpListener, TcpStream},
    process::{Command, Stdio},
};

use crate::clipboard::is_wayland;

pub fn main(_args: env::Args) -> Result<()> {
    let addr = "127.0.0.1:8022";
    let listener = TcpListener::bind(addr).with_context(|| format!("binding to {}", addr))?;

    while let Ok((conn, addr)) = listener.accept() {
        if let Err(err) = handle_conn(conn) {
            eprintln!("error: handling connection from {}: {}", addr, err)
        }
    }

    Ok(())
}

fn handle_conn(mut conn: TcpStream) -> Result<()> {
    let mut action_byte = [0u8; 1];
    conn.read_exact(&mut action_byte)
        .context("reading action byte")?;
    let header = Header::from(action_byte[0]);

    eprintln!("[debug] received header: {:?}", header);

    match header.action {
        Action::Paste => {
            let mut cmd = if is_wayland() {
                Command::new("xsel")
                    .args(header.selection.to_xsel_flag())
                    .arg("--output")
                    .stdout(Stdio::piped())
                    .spawn()
                    .context("running xsel")?
            } else {
                Command::new("wl-paste")
                    .arg("--no-newline")
                    .args(header.selection.to_wl_flag())
                    .stdout(Stdio::piped())
                    .spawn()
                    .context("running wl-paste")?
            };
            let mut stdout = cmd.stdout.take().unwrap();
            let copy_err = io::copy(&mut stdout, &mut conn).context("");

            let status = cmd.wait().context("waiting for xsel to close")?;
            match status.code() {
                Some(0) => {
                    copy_err?;
                }
                Some(code) => bail!("xsel exited with code: {}", code),
                None => bail!("xsel failed for an unknown reason"),
            }
        }
        Action::Copy => {
            let mut cmd = if is_wayland() {
                Command::new("xsel")
                    .args(header.selection.to_xsel_flag())
                    .arg("--input")
                    .stdin(Stdio::piped())
                    .spawn()
                    .context("running xsel")?
            } else {
                Command::new("wl-copy")
                    .args(header.selection.to_wl_flag())
                    .stdin(Stdio::piped())
                    .spawn()
                    .context("running wl-copy")?
            };
            let mut stdin = cmd.stdin.take().unwrap();
            let copy_err = io::copy(&mut conn, &mut stdin).context("");
            drop(stdin);

            let status = cmd.wait().context("waiting for xsel to close")?;
            match status.code() {
                Some(0) => {
                    copy_err?;
                }
                Some(code) => bail!("xsel exited with code: {}", code),
                None => bail!("xsel failed for an unknown reason"),
            }
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    Copy = 0,
    Paste = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Selection {
    Primary = 0,
    Clipboard = 1,
}

impl Selection {
    pub fn to_xsel_flag(self) -> Vec<&'static str> {
        match self {
            Selection::Primary => vec!["--primary"],
            Selection::Clipboard => vec!["--clipboard"],
        }
    }

    pub fn to_wl_flag(self) -> Vec<&'static str> {
        match self {
            Selection::Primary => vec!["--primary"],
            Selection::Clipboard => vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Header {
    pub action: Action,
    pub selection: Selection,
}

impl From<u8> for Header {
    fn from(byte: u8) -> Header {
        let action = match byte & 0b1 {
            0 => Action::Copy,
            _ => Action::Paste,
        };
        let selection = match (byte >> 1) & 0b1 {
            0 => Selection::Primary,
            _ => Selection::Clipboard,
        };
        Header { action, selection }
    }
}

impl From<Header> for u8 {
    fn from(header: Header) -> u8 {
        let action_bit = header.action as u8;
        let selection_bit = header.selection as u8;
        action_bit | (selection_bit << 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_serialization() {
        for action in [Action::Copy, Action::Paste] {
            for selection in [Selection::Primary, Selection::Clipboard] {
                let header = Header { action, selection };
                assert_eq!(header, Header::from(u8::from(header)));
            }
        }
    }
}
