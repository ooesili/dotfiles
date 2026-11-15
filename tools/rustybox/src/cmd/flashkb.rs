use crate::shell::CommandExt;
use anyhow::{Context, Result, bail};
use std::{
    env,
    process::{Command, Stdio},
    thread,
    time::{Duration, Instant},
};

pub fn main(_args: env::Args) -> Result<()> {
    println!("==> compiling");
    Command::new("qmk")
        .arg("compile")
        .run()
        .context("compiling")?;

    println!("waiting for keyboard to be in bootloader mode");
    wait_for_controller()?;

    println!("==> erasing memory");
    Command::new("dfu-programmer")
        .args(["atmega32u4", "erase", "--force"])
        .run()
        .context("erasing memory")?;

    println!("==> flashing firmware");
    Command::new("dfu-programmer")
        .args(["atmega32u4", "flash", "v60_type_r_websley.hex"])
        .run()
        .context("flashing memory")?;

    println!("==> resetting controller");
    Command::new("dfu-programmer")
        .args(["atmega32u4", "reset"])
        .run()
        .context("resetting controller")?;

    println!("==> done");
    Ok(())
}

fn wait_for_controller() -> Result<()> {
    let timeout = Duration::from_secs(120);
    let deadline = Instant::now().checked_add(timeout).unwrap();
    let interval = Duration::from_secs(1);
    while !Command::new("dfu-programmer")
        .args(["atmega32u4", "get", "bootloader-version"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .try_run()?
    {
        if Instant::now() > deadline {
            bail!("timed out");
        }
        thread::sleep(interval);
    }

    Ok(())
}
