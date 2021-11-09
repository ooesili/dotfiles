use anyhow::{bail, Context, Result};
use std::{env, fs, path::PathBuf, str};

pub fn main(mut args: env::Args) -> Result<()> {
    match args.next() {
        Some(cmd) if cmd == "play" => play(),
        Some(cmd) if cmd == "pause" => pause(),
        Some(cmd) if cmd == "toggle" => toggle(),
        Some(cmd) if cmd == "stop" => stop(),
        Some(cmd) => bail!("unknown command: {}", cmd),
        None => bail!("no command given"),
    }
}

fn play() -> Result<()> {
    let players = playerctl::list()?;
    play_last_of(&players)
}

fn toggle() -> Result<()> {
    let players = playerctl::list()?;
    for player in players.iter() {
        if let playerctl::Status::Playing = playerctl::status(player)? {
            playerctl::pause(player)?;
            write_last_touched(player)?;
            return Ok(());
        }
    }

    play_last_of(&players)
}

fn play_last_of(players: &[String]) -> Result<()> {
    if let Some(cache_file) = cache_file() {
        let last_player = String::from_utf8(fs::read(cache_file)?)?;
        for player in players {
            if player == &last_player {
                playerctl::play(player)?;
                return Ok(());
            }
        }
    }

    if let Some(player) = players.first() {
        playerctl::play(player)?;
    }

    Ok(())
}

fn pause() -> Result<()> {
    stop_with(playerctl::pause)
}

fn stop() -> Result<()> {
    stop_with(playerctl::stop)
}

fn stop_with(f: fn(&str) -> Result<()>) -> Result<()> {
    for player in playerctl::list()?.iter() {
        if let playerctl::Status::Playing = playerctl::status(player)? {
            f(player)?;
            write_last_touched(player)?;
            break;
        }
    }

    Ok(())
}

fn write_last_touched(player: &str) -> Result<()> {
    if let Some(cache_file) = cache_file() {
        std::fs::write(cache_file, player).context("saving which player was touched last")?;
    }
    Ok(())
}

fn cache_file() -> Option<PathBuf> {
    dirs::cache_dir().map(|dir| dir.join("mediactl-last_player"))
}

mod playerctl {
    use anyhow::{bail, ensure, Result};
    use std::{
        convert::{TryFrom, TryInto},
        io::{self, BufRead},
        process::Command,
        str,
    };

    const fn playerctl_bin() -> &'static str {
        match option_env!("PLAYERCTL_BIN") {
            Some(path) => path,
            None => "playerctl",
        }
    }

    pub fn list() -> Result<Vec<String>> {
        let output = Command::new(playerctl_bin()).arg("-l").output()?;
        ensure!(output.status.success(), "failed listing players");
        Ok(output.stdout.lines().collect::<io::Result<Vec<_>>>()?)
    }

    pub fn status(player: &str) -> Result<Status> {
        let output = Command::new(playerctl_bin())
            .arg("status")
            .arg("--player")
            .arg(player)
            .output()?;
        ensure!(
            output.status.success(),
            "failed getting status of player: {}",
            player
        );
        str::from_utf8(&output.stdout)?.trim_end().try_into()
    }

    pub fn play(player: &str) -> Result<()> {
        player_command("play", player)
    }

    pub fn pause(player: &str) -> Result<()> {
        player_command("pause", player)
    }

    pub fn stop(player: &str) -> Result<()> {
        player_command("stop", player)
    }

    fn player_command(command: &'static str, player: &str) -> Result<()> {
        let exit_status = Command::new(playerctl_bin())
            .arg(command)
            .arg("--player")
            .arg(player)
            .status()?;
        ensure!(
            exit_status.success(),
            "failed to {} player: {}",
            command,
            player
        );
        Ok(())
    }

    #[derive(Debug)]
    pub enum Status {
        Playing,
        Paused,
        Stopped,
    }

    impl TryFrom<&str> for Status {
        type Error = anyhow::Error;

        fn try_from(s: &str) -> Result<Self> {
            Ok(match s {
                "Playing" => Self::Playing,
                "Paused" => Self::Paused,
                "Stopped" => Self::Stopped,
                s => bail!("unknown player status: {:?}", s),
            })
        }
    }
}
