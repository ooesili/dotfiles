local colors = {
  bg = '#282828',
  alt_bg = '#383838',
  dark_fg = '#585858',
  fg = '#b8b8b8',
  light_fg = '#d8d8d8',
  normal = '#7cafc2',
  insert = '#a1b56c',
  visual = '#ba8baf',
  replace = '#dc9656',
}

return {
  normal = {
    a = { fg = colors.bg, bg = colors.normal },
    b = { fg = colors.light_fg, bg = colors.alt_bg },
    c = { fg = colors.fg, bg = colors.bg },
  },
  replace = {
    a = { fg = colors.bg, bg = colors.replace },
    b = { fg = colors.light_fg, bg = colors.alt_bg },
  },
  insert = {
    a = { fg = colors.bg, bg = colors.insert },
    b = { fg = colors.light_fg, bg = colors.alt_bg },
  },
  visual = {
    a = { fg = colors.bg, bg = colors.visual },
    b = { fg = colors.light_fg, bg = colors.alt_bg },
  },
  inactive = {
    a = { fg = colors.dark_fg, bg = colors.bg },
    b = { fg = colors.dark_fg, bg = colors.bg },
    c = { fg = colors.dark_fg, bg = colors.bg },
  }
}
