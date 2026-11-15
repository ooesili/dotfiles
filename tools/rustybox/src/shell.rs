use anyhow::{Context, Result, ensure};
use std::process::Command;

pub trait CommandExt {
    fn try_run(&mut self) -> Result<bool>;

    fn run(&mut self) -> Result<()> {
        let ok = self.try_run()?;
        ensure!(ok, "comand exited with non-zero status");
        Ok(())
    }
}

impl CommandExt for Command {
    fn try_run(&mut self) -> Result<bool> {
        Ok(self
            .spawn()
            .context("starting command")?
            .wait()
            .context("waiting for command")?
            .success())
    }
}
