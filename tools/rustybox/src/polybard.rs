use std::{
    collections::HashMap,
    env, io,
    path::PathBuf,
    process::{Child, Command},
    sync::mpsc,
    thread,
};

use anyhow::{Context, Result};
use i3_ipc::{event::Subscribe, reply::Output, Connect, I3Stream, I3};
use signal_hook::{consts::signal, iterator::Signals};

pub fn main(_args: env::Args) -> Result<()> {
    let prefs = Preferences {
        main_output: env::var("POLYBARD_MAIN_OUTPUT").context("POLYBARD_MAIN_OUTPUT not set")?,
        host: env::var("POLYBARD_HOST").context("POLYBARD_HOST not set")?,
    };

    let (sender, receiver) = mpsc::channel();

    let data_home = env::var("XDG_DATA_HOME").context("XDG_DATA_HOME not set")?;

    let mut state_file = PathBuf::from(data_home);
    state_file.push("polybard/state");
    let bars = Bars::new(state_file);

    let mut signals =
        Signals::new([signal::SIGTERM, signal::SIGINT]).context("installing signal handlers")?;

    watch_monitors(sender.clone(), prefs)?;

    thread::spawn(move || {
        signals.into_iter().next();
        sender.send(Msg::Quit).unwrap();
    });

    handle_msgs(bars, receiver)
}

fn handle_msgs(mut bars: Bars, receiver: mpsc::Receiver<Msg>) -> Result<()> {
    for msg in receiver {
        match msg {
            Msg::MonitorChange(new_bars) => bars.sync(new_bars),
            Msg::Quit => {
                eprintln!("received signal, shutting down");
                break;
            }
        }
    }
    Ok(())
}

fn watch_monitors(sender: mpsc::Sender<Msg>, prefs: Preferences) -> Result<()> {
    let mut i3 = I3::connect().context("connecting to i3")?;

    i3.subscribe([Subscribe::Output])
        .context("subscribing to output changes")?;

    thread::spawn(move || -> Result<()> {
        loop {
            let bars =
                current_bars(&mut i3, &prefs).context("gettings list of monitors from i3")?;
            if sender.send(Msg::MonitorChange(bars)).is_err() {
                return Ok(());
            }
            i3.receive_event().context("receiving event from i3")?;
        }
    });

    Ok(())
}

#[derive(Debug)]
struct Bars {
    bars: HashMap<Bar, BarState>,
    state_file: PathBuf,
}

impl Bars {
    fn new(state_file: PathBuf) -> Bars {
        Bars {
            bars: HashMap::new(),
            state_file,
        }
    }

    fn sync(&mut self, new_bars: impl IntoIterator<Item = Bar>) {
        // TODO: use me
        _ = self.state_file;

        for new_bar in new_bars {
            if self.bars.contains_key(&new_bar) {
                continue;
            }

            let state = Command::new("polybar")
                .arg("-r")
                .arg(&new_bar.name)
                .env("MONITOR", &new_bar.monitor)
                .spawn()
                .map(BarState::Running)
                .unwrap_or_else(|err| {
                    eprintln!("error: bar {} failed to start: {}", new_bar.name, err);
                    BarState::Failed(err)
                });

            self.bars.insert(new_bar, state);
        }
    }
}

impl Drop for Bars {
    fn drop(&mut self) {
        for (bar, state) in self.bars.drain() {
            let mut child = match state {
                BarState::Running(child) => child,
                BarState::Failed(_) => continue,
            };

            if let Err(err) = child.kill() {
                eprintln!("error: killing bar {}: {}", bar.name, err);
                continue;
            }
            if let Err(err) = child.wait() {
                eprintln!("error: waiting for bar '{}' to exit: {}", bar.name, err);
            }
        }
    }
}

#[derive(Debug)]
enum BarState {
    Running(Child),
    Failed(io::Error),
}

fn current_bars(i3: &mut I3Stream, prefs: &Preferences) -> Result<Vec<Bar>> {
    let outputs = i3.get_outputs()?;
    let bars = outputs
        .into_iter()
        .filter_map(|o| o.active.then(|| output_to_bar(o, prefs)))
        .collect::<Vec<_>>();
    Ok(bars)
}

enum Msg {
    MonitorChange(Vec<Bar>),
    Quit,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Bar {
    name: String,
    monitor: String,
}

fn output_to_bar(output: Output, prefs: &Preferences) -> Bar {
    let name = if output.name == prefs.main_output {
        format!("{}-main", prefs.host)
    } else {
        format!("{}-alt", prefs.host)
    };

    Bar {
        name,
        monitor: output.name,
    }
}

struct Preferences {
    main_output: String,
    host: String,
}
