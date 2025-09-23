use anyhow::{ensure, Context, Result};
use serde::{Deserialize, Serialize};
use std::env;
use std::process::Command;

pub fn main(_args: env::Args) -> Result<()> {
    let output = Command::new("swaymsg")
        .args(["-t", "get_tree"])
        .output()
        .context("running swaymsg")?;
    ensure!(output.status.success(), "swaymsg command failed");

    let tree: SwayTree =
        serde_json::from_slice(&output.stdout).context("deserializing swaymsg stdout")?;

    print_names(tree.nodes);
    print_names(tree.floating_nodes);

    Ok(())
}

fn print_names(nodes: impl IntoIterator<Item = SwayNode>) {
    for node in nodes {
        if node.is_normal_window() {
            if let Some(ref name) = node.name {
                println!("{}\t{} - {}", node.id, node.app_id(), name);
            }
        }
        print_names(node.nodes);
        print_names(node.floating_nodes);
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct SwayTree {
    nodes: Vec<SwayNode>,
    floating_nodes: Vec<SwayNode>,
}

#[derive(Debug, Serialize, Deserialize)]
struct SwayNode {
    id: i32,
    app_id: Option<String>,
    #[serde(rename = "type")]
    node_type: String,
    name: Option<String>,
    orientation: String,
    nodes: Vec<SwayNode>,
    floating_nodes: Vec<SwayNode>,
    window_properties: Option<WindowProperties>,
}

#[derive(Debug, Serialize, Deserialize)]
struct WindowProperties {
    class: String,
}

impl SwayNode {
    fn is_normal_window(&self) -> bool {
        (self.node_type == "con" || self.node_type == "floating_con") && self.orientation == "none"
    }

    fn app_id(&self) -> &str {
        if let Some(ref app_id) = self.app_id {
            // wayland window
            app_id
        } else if let Some(ref props) = self.window_properties {
            // xwayland window
            &props.class
        } else {
            panic!("no app_id or window_properties.class on window")
        }
    }
}
