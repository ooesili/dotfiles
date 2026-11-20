mod eventno;
mod msgno;

use std::{
    env,
    io::{Read, Write},
    os::unix::net::UnixStream,
};

use anyhow::{Context, Result, anyhow, bail, ensure};
use serde::Deserialize;

#[derive(Debug)]
pub struct Sway {
    stream: UnixStream,
}

impl Sway {
    pub fn connect() -> Result<Sway> {
        let stream = sway_socket()?;
        Ok(Sway { stream })
    }

    pub fn run_command(&mut self, command: impl AsRef<[u8]>) -> Result<()> {
        send(&mut self.stream, msgno::RUN_COMMAND, command.as_ref()).context("sending command")?;
        let (ty, payload) = recv(&mut self.stream).context("reading reply")?;
        ensure!(ty == msgno::RUN_COMMAND);
        let replies: Vec<RunCommandReply> =
            serde_json::from_slice(&payload).context("parsing reply payload as json")?;

        // TODO: aggregate errors
        for reply in replies {
            if !reply.success {
                let prefix = match reply.parse_error {
                    true => "parse error",
                    false => "error",
                };
                let msg = match reply.error {
                    Some(ref err) => err,
                    None => "<no error message>",
                };
                bail!("{}: {}", prefix, msg);
            }
        }

        Ok(())
    }

    pub fn subscribe(&mut self) -> Result<Events> {
        let mut stream = sway_socket()?;
        let events = serde_json::to_vec(&["window"]).unwrap();
        send(&mut stream, msgno::SUBSCRIBE, &events)?;

        let (ty, payload) = recv(&mut stream)?;
        ensure!(ty == msgno::SUBSCRIBE, "unexpected message type: {}", ty);
        let reply: SubscribeReply =
            serde_json::from_slice(&payload).context("reading subscribe reply")?;
        ensure!(reply.success, "subscribe command did not succeed");

        Ok(Events { stream })
    }

    pub fn get_tree(&mut self) -> Result<Node> {
        send(&mut self.stream, msgno::GET_TREE, &[])?;
        let (ty, payload) = recv(&mut self.stream)?;
        ensure!(ty == msgno::GET_TREE, "unexpected message type: {}", ty);
        serde_json::from_slice(&payload).context("reading get_tree reply")
    }
}

fn sway_socket() -> Result<UnixStream> {
    let socket_path = env::var("SWAYSOCK").context("reading SWAYSOCK env var")?;
    UnixStream::connect(socket_path).context("connecting to unix socket")
}

fn recv(stream: &mut UnixStream) -> Result<(u32, Vec<u8>)> {
    let mut header = [0u8; 14]; // 6 magic + 4 length + 4 type
    stream.read_exact(&mut header)?;
    ensure!(&header[..6] == MAGIC, "bad magic string");

    let len = u32::from_ne_bytes(header[6..10].try_into().unwrap()) as usize;
    let msg_type = u32::from_ne_bytes(header[10..14].try_into().unwrap());

    let mut payload = vec![0u8; len];
    stream.read_exact(&mut payload)?;
    Ok((msg_type, payload))
}

fn send(stream: &mut UnixStream, msg_type: u32, payload: &[u8]) -> Result<()> {
    stream.write_all(MAGIC)?;
    stream.write_all(&(payload.len() as u32).to_ne_bytes())?;
    stream.write_all(&msg_type.to_ne_bytes())?;
    stream.write_all(payload)?;
    Ok(())
}

const MAGIC: &[u8; 6] = b"i3-ipc";

#[derive(Debug)]
#[non_exhaustive]
pub struct Events {
    stream: UnixStream,
}

impl Iterator for Events {
    type Item = Result<Event>;

    fn next(&mut self) -> Option<Self::Item> {
        let (ty, payload) = match recv(&mut self.stream) {
            Ok(event) => event,
            Err(err) => return Some(Err(err)),
        };
        if ty != eventno::WINDOW {
            return Some(Err(anyhow!("unexpected event number: {}", ty)));
        }

        // let event = serde_json::from_slice(&payload).context("deserializing line from swaymsg")?;
        let event = serde_json::from_slice(&payload).unwrap();
        Some(Ok(Event::Window(event)))
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Event {
    Window(WindowEvent),
}

#[derive(Debug, Deserialize)]
pub struct WindowEvent {
    pub change: WindowChange,
    pub container: WindowContainer,
}

#[derive(Debug, Deserialize)]
#[non_exhaustive]
#[allow(unused)]
pub struct WindowContainer {
    pub app_id: String,
    pub window_rect: Rect,
    pub deco_rect: Rect,
    pub geometry: Rect,
    pub id: i32,
}

#[derive(Debug, Deserialize, Default)]
#[allow(unused)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
}

#[derive(Debug, Deserialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum WindowChange {
    New,
    Close,
    Focus,
    Title,
    FullscreenMode,
    Move,
    Floating,
    Urgent,
    Mark,
    // If sway adds a change type we don't know about yet.
    #[serde(other)]
    Unknown,
}

#[derive(Debug, Deserialize)]
struct SubscribeReply {
    success: bool,
}

#[derive(Debug, Deserialize)]
struct RunCommandReply {
    pub success: bool,
    #[serde(default)]
    pub parse_error: bool,
    #[serde(default)]
    pub error: Option<String>,
}

#[derive(Debug, Deserialize, Default)]
#[allow(unused)]
pub struct Node {
    pub id: i32,
    pub name: Option<String>,
    #[serde(rename = "type")]
    pub node_type: String,
    pub border: String,
    pub current_border_width: i32,
    pub layout: String,
    pub orientation: String,
    pub percent: Option<f64>,
    pub rect: Rect,
    pub window_rect: Rect,
    pub deco_rect: Rect,
    pub geometry: Rect,
    pub urgent: bool,
    pub sticky: bool,
    pub marks: Vec<String>,
    pub focused: bool,
    pub focus: Vec<i32>,
    pub nodes: Vec<Node>,
    pub floating_nodes: Vec<Node>,
    pub representation: Option<String>,
    pub fullscreen_mode: i32,
    pub floating: Option<String>,
    pub scratchpad_state: Option<String>,
    pub app_id: Option<String>,
    pub pid: Option<i32>,
    pub foreign_toplevel_identifier: Option<String>,
    pub visible: Option<bool>,
    pub shell: Option<String>,
    pub inhibit_idle: Option<bool>,
    pub idle_inhibitors: Option<Inhibitors>,
    pub sandbox_engine: Option<String>,
    pub sandbox_app_id: Option<String>,
    pub sandbox_instance_id: Option<String>,
    pub tag: Option<String>,
    pub window: Option<i32>,
    pub window_properties: Option<WindowProperties>,
}

#[derive(Debug, Deserialize)]
#[allow(unused)]
pub struct Inhibitors {
    pub user: String,
    pub application: String,
}

#[derive(Debug, Deserialize)]
#[allow(unused)]
pub struct WindowProperties {
    pub title: String,
    pub class: String,
    pub instance: String,
    pub window_role: String,
    pub window_type: String,
    pub transient_for: String,
}
