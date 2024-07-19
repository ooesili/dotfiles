use std::{env, process::Command};

use anyhow::{bail, Context, Result};

use crate::rclipd::Selection;

pub trait Clipboard {
    fn copy(&self, selection: Selection) -> Result<()>;
    fn paste(&self, selection: Selection) -> Result<()>;
}

pub fn is_wayland() -> bool {
    env::var("XDG_SESSION_TYPE").is_ok_and(|s| s == "wayland")
}

pub fn get_current_clipboard() -> Box<dyn Clipboard> {
    if is_wayland() {
        Box::new(WlClipboard)
    } else {
        Box::new(Xsel)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Xsel;

impl Clipboard for Xsel {
    fn copy(&self, selection: Selection) -> Result<()> {
        let status = Command::new("xsel")
            .arg("--input")
            .args(selection.to_xsel_flag())
            .spawn()
            .context("running xsel")?
            .wait()
            .context("waiting for xsel to stop")?;
        if !status.success() {
            bail!("xsel failed with status {}", status.code().unwrap_or(-1));
        }

        Ok(())
    }

    fn paste(&self, selection: Selection) -> Result<()> {
        let status = Command::new("xsel")
            .arg("--output")
            .args(selection.to_xsel_flag())
            .spawn()
            .context("running xsel")?
            .wait()
            .context("waiting for xsel to stop")?;

        match status.code() {
            Some(0) => {}
            Some(code) => bail!("xsel exited with code: {}", code),
            None => bail!("xsel failed for an unknown reason"),
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct WlClipboard;

impl Clipboard for WlClipboard {
    fn copy(&self, selection: Selection) -> Result<()> {
        let status = Command::new("wl-copy")
            .args(selection.to_wl_flag())
            .spawn()
            .context("running wl-copy")?
            .wait()
            .context("waiting for wl-copy to stop")?;
        if !status.success() {
            bail!("wl-copy failed with status {}", status.code().unwrap_or(-1));
        }

        Ok(())
    }

    fn paste(&self, selection: Selection) -> Result<()> {
        let status = Command::new("wl-paste")
            .arg("--no-newline")
            .args(selection.to_wl_flag())
            .spawn()
            .context("running wl-paste")?
            .wait()
            .context("waiting for wl-paste to stop")?;

        match status.code() {
            Some(0) => {}
            Some(code) => bail!("wl-paste exited with code: {}", code),
            None => bail!("wl-paste failed for an unknown reason"),
        }

        Ok(())
    }
}
