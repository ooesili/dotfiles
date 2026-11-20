use std::{env, fs::remove_file, io::ErrorKind, os::unix::fs::symlink, path, process::Command};

use anyhow::{Context, Result, anyhow};
use dirs::home_dir;

use crate::shell::CommandExt;

pub fn main(mut args: env::Args) -> Result<()> {
    let image_path = args.next().ok_or_else(|| anyhow!("not enough arguments"))?;
    let image_path = path::absolute(image_path).context("getting absolute value of image")?;

    let wallpaper_link = home_dir()
        .ok_or_else(|| anyhow!("cannot determine home directory"))?
        .join(".wallpaper");

    if let Err(err) = remove_file(&wallpaper_link)
        && err.kind() != ErrorKind::NotFound
    {
        return Err(err.into());
    }
    symlink(image_path, &wallpaper_link).context("creating symlink")?;
    Command::new("swaymsg")
        .args(["output", "*", "bg"])
        .arg(wallpaper_link)
        .arg("fill")
        .run()
        .context("running swaymsg")
}
