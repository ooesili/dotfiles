{
  lib,
  writeTextDir,
  symlinkJoin,
}: let
  tmuxTheme = name: theme:
    writeTextDir "kanagawa-tmux-${name}.conf" ''
      set -g message-command-style bg=colour11,fg=colour7
      set -g message-style fg=colour7,bg=${theme.ui.bg_gutter}
      set -g pane-active-border-style fg=colour4
      set -g pane-border-style fg=${theme.ui.nontext}
      set -g status "on"
      set -g status-style none
      set -g status-bg "${theme.ui.bg_gutter}"
      set -g status-justify "centre"
      set -g status-left-style none
      set -g status-left-length "100"
      set -g status-right-style none
      set -g status-right-length "100"
      setw -g window-status-style fg=colour7,none
      setw -g window-status-activity-style bg=colour10,none,fg=colour4
      setw -g window-status-separator ""
      setw -g window-status-style bg=colour10
      set -g status-left "#[fg=${theme.ui.bg_m3},bg=colour4,bold] #S #[fg=${theme.syn.fun},bg=${theme.ui.bg_p2},nobold] #F #[fg=${theme.ui.fg_dim},bg=${theme.ui.bg_gutter}] #W "
      set -g status-right " #[fg=${theme.syn.fun},bg=${theme.ui.bg_p2}] %a %b %d | %R #[fg=${theme.ui.bg_m3},bg=colour4] #H "
      setw -g window-status-format "#[fg=${theme.ui.fg_dim},bg=${theme.ui.bg_gutter}] #I | #W "
      setw -g window-status-current-format "#[fg=${theme.ui.fg},bg=${theme.ui.bg_p2},bold] #I | #W "
    '';

  themes = lib.importJSON ./kanagawa.json;
in
  symlinkJoin {
    name = "kanagawa-tmux-themes";
    paths = lib.mapAttrsToList tmuxTheme themes;
  }
