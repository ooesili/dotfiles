use anyhow::{Context, Ok, Result};
use serde::Deserialize;
use std::env;

use crate::swaymsg::{Event, Node, Sway, WindowChange};

#[derive(Debug, Deserialize)]
pub struct WindowEvent {
    pub change: String,
    pub container: WindowContainer,
}

#[derive(Debug, Deserialize)]
pub struct WindowContainer {
    pub app_id: String,
}

pub fn main(_args: env::Args) -> Result<()> {
    let mut sway = Sway::connect().context("connecting to sway")?;
    let events = sway.subscribe().context("subscribing to events")?;

    for event in events {
        let event = event.context("reading event")?;
        match event {
            Event::Window(ref window_event) => {
                if window_event.change == WindowChange::New
                    && window_event.container.app_id == "com.mitchellh.ghostty"
                {
                    let root_node = sway.get_tree()?;
                    if !should_resize(&mut vec![], &root_node, window_event.container.id) {
                        continue;
                    }
                    let width = window_event.container.geometry.width;
                    const TARGET_WIDTH: i32 = 400;
                    if width < TARGET_WIDTH {
                        sway.run_command(format!("resize grow width {}px", TARGET_WIDTH - width))?;
                    } else {
                        sway.run_command(format!(
                            "resize shrink width {}px",
                            width - TARGET_WIDTH
                        ))?;
                    }
                }
            }
        }
    }

    Ok(())
}

fn should_resize(breadcrumbs: &mut Vec<BreadCrumb>, node: &Node, target_window_id: i32) -> bool {
    for (i, child) in node.nodes.iter().enumerate() {
        breadcrumbs.push(BreadCrumb {
            layout: node.layout.clone(),
            children: node.nodes.len(),
            index: i,
            is_workspace: node.node_type == "workspace",
            output_width: (node.node_type == "output").then_some(node.rect.width),
        });
        if should_resize(breadcrumbs, child, target_window_id) {
            return true;
        }
        breadcrumbs.pop();
    }
    if node.id != target_window_id {
        return false;
    }

    let Some(crumb) = breadcrumbs.last() else {
        return false;
    };
    let workspace_child = crumb.is_workspace == true
        || breadcrumbs
            .iter()
            .rev()
            .skip(1)
            .skip_while(|b| !b.is_workspace && b.children == 1)
            .next()
            .map(|b| b.is_workspace)
            .unwrap_or(false);

    let on_wide_monitor = breadcrumbs
        .iter()
        .rev()
        .any(|b| b.output_width.map(|n| n >= 2560).unwrap_or(false));

    crumb.children == 2
        && crumb.index == 1
        && crumb.layout == "splith"
        && workspace_child
        && on_wide_monitor
}

#[derive(Debug)]
struct BreadCrumb {
    layout: String,
    is_workspace: bool,
    children: usize,
    index: usize,
    output_width: Option<i32>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::swaymsg::{Node, Rect};

    #[test]
    fn test_should_resize() {
        let root_node = Node {
            nodes: vec![
                Node {
                    node_type: "output".to_string(),
                    rect: Rect {
                        x: 0,
                        y: 0,
                        width: 2560,
                        height: 1440,
                    },
                    nodes: vec![
                        Node {
                            node_type: "workspace".to_string(),
                            layout: "splith".to_string(),
                            nodes: vec![
                                Node { ..Node::default() },
                                Node {
                                    id: 1,
                                    ..Node::default()
                                },
                            ],
                            ..Node::default()
                        },
                        Node {
                            node_type: "workspace".to_string(),
                            layout: "splith".to_string(),
                            nodes: vec![
                                Node {
                                    id: 2,
                                    ..Node::default()
                                },
                                Node { ..Node::default() },
                            ],
                            ..Node::default()
                        },
                        Node {
                            node_type: "workspace".to_string(),
                            layout: "splitv".to_string(),
                            nodes: vec![
                                Node { ..Node::default() },
                                Node {
                                    id: 3,
                                    ..Node::default()
                                },
                            ],
                            ..Node::default()
                        },
                        Node {
                            node_type: "workspace".to_string(),
                            layout: "splith".to_string(),
                            nodes: vec![Node {
                                id: 4,
                                ..Node::default()
                            }],
                            ..Node::default()
                        },
                        Node {
                            node_type: "workspace".to_string(),
                            layout: "splitv".to_string(),
                            nodes: vec![Node {
                                layout: "splith".to_string(),
                                nodes: vec![
                                    Node { ..Node::default() },
                                    Node {
                                        id: 5,
                                        ..Node::default()
                                    },
                                ],
                                ..Node::default()
                            }],
                            ..Node::default()
                        },
                    ],
                    ..Node::default()
                },
                Node {
                    node_type: "output".to_string(),
                    rect: Rect {
                        x: 0,
                        y: 0,
                        width: 1920,
                        height: 1080,
                    },
                    nodes: vec![Node {
                        node_type: "workspace".to_string(),
                        layout: "splith".to_string(),
                        nodes: vec![
                            Node { ..Node::default() },
                            Node {
                                id: 6,
                                ..Node::default()
                            },
                        ],
                        ..Node::default()
                    }],
                    ..Node::default()
                },
            ],
            ..Node::default()
        };

        assert_eq!(should_resize(&mut vec![], &root_node, 1), true);
        assert_eq!(should_resize(&mut vec![], &root_node, 2), false);
        assert_eq!(should_resize(&mut vec![], &root_node, 3), false);
        assert_eq!(should_resize(&mut vec![], &root_node, 4), false);
        assert_eq!(should_resize(&mut vec![], &root_node, 5), true);
        assert_eq!(should_resize(&mut vec![], &root_node, 6), false);
    }
}
