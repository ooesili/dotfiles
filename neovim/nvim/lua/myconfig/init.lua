local function noremap(mode, lhs, rhs, opts)
  opts = opts or {}
  opts.noremap = true
  vim.keymap.set(mode, lhs, rhs, opts)
end

local function augroup(name, commands)
  vim.api.nvim_command('augroup ' .. name)
  vim.api.nvim_command('autocmd!')
  for _, autocmd in ipairs(commands) do
    vim.api.nvim_command('autocmd ' .. table.concat(autocmd, ' '))
  end
  vim.api.nvim_command('augroup END')
end

-- colors
vim.o.termguicolors = true
vim.cmd.colorscheme('kanagawa')

-- basics
vim.o.breakindent = true
vim.o.completeopt = 'menu,menuone,noselect'
vim.o.cursorline = true
vim.o.expandtab = true
vim.o.ignorecase = true
vim.o.linebreak = true
vim.o.number = true
vim.o.shiftwidth = 2
vim.o.smartcase = true
vim.o.softtabstop = 2
vim.o.tabstop = 2
vim.o.undofile = true
vim.o.wildmode = 'longest:full,full'
vim.o.winborder = 'rounded'

-- ripgrep
if vim.fn.executable('rg') == 1 then
  vim.o.grepprg = 'rg --vimgrep --no-heading'
  vim.o.grepformat = '%f:%l:%c:%m,%f:%l:%m'
end

-- yank and paste to system clipboard
vim.o.clipboard = 'unnamedplus'

-- mappings
vim.g.mapleader = ' '
noremap('n', '<C-e>', '5<C-e>')
noremap('n', '<C-y>', '5<C-y>')
noremap('n', 'g<C-e>', '<C-e>')
noremap('n', 'g<C-y>', '<C-y>')
noremap('n', 'g<C-l>', '<C-l>')
noremap('n', '<Leader>w', ':update<CR>')
noremap('n', '<Leader>c', ':Trouble quickfix<CR>')
noremap('n', '<Leader>x', ':TroubleToggle workspace_diagnostics<CR>')
noremap('n', '<Leader>gs', ':Git<CR>', { desc = 'Git: open status window' })
noremap('n', '<Leader>gS', ':Git!<CR>', { desc = 'Git: open small status window' })
noremap('n', '-', function() require('oil').open() end)
-- telescope
noremap('n', '<Leader>f', ':Telescope find_files<CR>', { desc = 'Find files' })
noremap('n', '<Leader>b', ':Telescope buffers<CR>', { desc = 'Browse open buffers' })
noremap('n', '<Leader>o', ':Telescope oldfiles<CR>', { desc = 'Recently opened files' })
noremap('n', '<Leader>s', ':Telescope live_grep<CR>', { desc = 'Live grep search' })
noremap('n', '<Leader>/', ':Telescope current_buffer_fuzzy_find<CR>', { desc = 'Search in current file' })
noremap('n', '<Leader>z', require('zoxide').telescope, { desc = 'Quick change directory' })
noremap('n', 'z=', ':Telescope spell_suggest<CR>', { desc = 'Spelling suggestions' })
-- window navigation
noremap('n', '<C-h>', '<C-w><C-h>')
noremap('n', '<C-j>', '<C-w><C-j>')
noremap('n', '<C-k>', '<C-w><C-k>')
noremap('n', '<C-l>', '<C-w><C-l>')

-- status line
require('lualine').setup({
  options = { theme = 'kanagawa' }
})

-- terminal
noremap('t', '<C-l>', '<nop>')
vim.o.scrollback = 10000
augroup('InitVimTerminalSettings', {
  { 'TermOpen', '*', 'setlocal', 'nonumber' }
})

-- extra filetypes
augroup('InitVimFiletypes', {
  { 'BufNewFile,BufRead', '.envrc',                'set',      'filetype=sh' },
  { 'BufNewFile,BufRead', 'Vagrantfile,Berksfile', 'set',      'filetype=ruby' },
  { 'BufNewFile,BufRead', 'tmux.conf',             'set',      'filetype=tmux.conf' },
  { 'BufNewFile,BufRead', '*.nomad',               'set',      'filetype=hcl' },
  { 'BufNewFile,BufRead', '*.tfstate',             'setlocal', 'filetype=json shiftwidth=4 softtabstop=4' },
})

-- vim-commentary extensions
augroup('InitVimComentary', {
  { 'FileType', 'tf',  'setlocal', 'commentstring=#\\ %s' },
  { 'FileType', 'sql', 'setlocal', 'commentstring=--\\ %s' }
})

-- direnv
augroup('InitVimDirenv', {
  { 'BufWritePost', '.envrc', 'silent', '!direnv allow %' }
})

-- terraform
vim.g.terraform_fmt_on_save = 1

-- trouble.nvim
require('trouble').setup()

-- telescope
local telescope_actions = require('telescope.actions')
local telescope = require('telescope')
telescope.setup({
  defaults = {
    focus = true,
    layout_strategy = 'flex',
    mappings = {
      i = {
        -- clear input instead of scrolling
        ["<C-u>"] = false,
        ["<Esc>"] = telescope_actions.close,
        ["<C-c>"] = function()
          vim.cmd('stopinsert')
        end,
        ["<C-q>"] = require("trouble.sources.telescope").open
      },
    },
  },
  extensions = {
    fzf = {},
    ["ui-select"] = {
      require("telescope.themes").get_cursor {}
    }
  },
})
telescope.load_extension('fzf')
telescope.load_extension('ui-select')

require('oil').setup({
  skip_confirm_for_simple_edits = true,
  keymaps = {
    ["<C-l>"] = false,
    ["<C-h>"] = false,
    ["g<C-l>"] = "actions.refresh",
    ["g<C-h>"] = { "actions.select", opts = { horizontal = true }, desc = "Open the entry in a horizontal split" },
  }
})

-- -- TODO: see if I can delete this
-- -- luasnip
-- local luasnip = require('luasnip')
-- require('luasnip.loaders.from_vscode').lazy_load()

-- blink.nvim
require('blink.cmp').setup({
  keymap = { preset = 'default' },

  appearance = {
    nerd_font_variant = 'mono'
  },

  -- (Default) Only show the documentation popup when manually triggered
  -- completion = { documentation = { auto_show = false } },

  signature = { enabled = true }
})

-- lsp
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(ev)
    local opts = { noremap = true, silent = true, buffer = ev.buf }

    -- Format on save
    vim.api.nvim_create_autocmd({ "BufWritePre" }, {
      buffer = ev.buf,
      callback = function() vim.lsp.buf.format() end,
    })

    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    vim.keymap.set('n', 'gd', ':Telescope lsp_definitions theme=cursor<CR>', opts)
    -- vim.keymap.set('n', '[d', ':lua vim.diagnostic.goto_prev()<CR>', opts)
    -- vim.keymap.set('n', ']d', ':lua vim.diagnostic.goto_next()<CR>', opts)
    vim.keymap.set('n', '[d', function() vim.diagnostic.jump({ count = -1, float = true }) end, opts)
    vim.keymap.set('n', ']d', function() vim.diagnostic.jump({ count = 1, float = true }) end, opts)
    vim.keymap.set('n', 'K', ':lua vim.lsp.buf.hover()<CR>', opts)
    vim.keymap.set('n', '<Leader>ln', ':lua vim.lsp.buf.rename()<CR>', opts)
    vim.keymap.set('n', '<Leader>la', ':lua vim.lsp.buf.code_action()<CR>', opts)
    vim.keymap.set('v', '<Leader>la', ':lua vim.lsp.buf.range_code_action()<CR>', opts)
    vim.keymap.set('n', '<Leader>lr', ':Telescope lsp_references theme=cursor<CR>', opts)
    vim.keymap.set('n', '<Leader>ld', ':Telescope diagnostics<CR>', opts)
    vim.keymap.set('n', '<Leader>lt', ':Telescope lsp_type_definitions<CR>', opts)
    vim.keymap.set('n', '<Leader>ls', ':Telescope lsp_document_symbols<CR>', opts)
    vim.keymap.set('n', '<Leader>lS', ':Telescope lsp_workspace_symbols<CR>', opts)
  end
})

-- language server settings
vim.lsp.enable({
  'bashls',
  'clangd',
  'expert',
  'gopls',
  'lua_ls',
  'nil_ls',
  'pyright',
  'rust_analyzer',
  'ts_ls',
  'zls'
})

-- better whitespace
vim.g.better_whitespace_filetypes_blacklist = {
  'diff', 'gitcommit', 'unite', 'qf', 'help', 'git'
}
vim.api.nvim_command('autocmd FileType fugitive DisableWhitespace')

-- change into the directory of the current file
vim.api.nvim_create_user_command('CD', function()
  vim.api.nvim_command("lcd " .. vim.fn.expand("%:h"))
end, {})

-- treesitter
require('nvim-treesitter').setup {
  highlight = { enable = true },
  incremental_selection = { enable = true },
  textobjects = { enable = true },

  -- -- dont' run regex parsing at the same time
  additional_vim_regex_highlighting = false,
}
vim.api.nvim_create_autocmd('FileType', {
  callback = function(args)
    -- Detection isn't automatic (or supported???) anymore, so I found a workaround:
    -- https://github.com/nvim-treesitter/nvim-treesitter/pull/8434/files
    local lang = vim.treesitter.language.get_lang(args.match)
    if not lang or not vim.treesitter.query.get(lang, 'highlights') then
      return
    end

    -- syntax highlighting, provided by Neovim
    vim.treesitter.start()
    -- folds, provided by Neovim
    vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
    vim.wo.foldmethod = 'expr'
    -- indentation, provided by nvim-treesitter
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})

vim.o.foldmethod = 'expr'
vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
vim.o.foldenable = false

-- nix
vim.api.nvim_command('autocmd BufRead,BufNewFile flake.lock setfiletype json')

-- fugitive
vim.g.fugitive_pty = 0

-- rclip
if vim.fn.executable('rclip') == 1 then
  vim.g.clipboard = {
    name = 'rclip',
    copy = {
      ['*'] = { 'rclip', 'copy', '--primary' },
      ['+'] = { 'rclip', 'copy', '--clipboard' }
    },
    paste = {
      ['*'] = { 'rclip', 'paste', '--primary' },
      ['+'] = { 'rclip', 'paste', '--clipboard' }
    }
  }
end

-- illuminate
require('illuminate').configure({
  filetypes_denylist = { 'fugitive', 'NvimTree', 'TelescopePrompt' }
})
vim.api.nvim_command(string.format('hi def IlluminatedWordText guibg=#%s', vim.g.base16_gui02))
vim.api.nvim_command(string.format('hi def IlluminatedWordRead guibg=#%s', vim.g.base16_gui02))
vim.api.nvim_command(string.format('hi def IlluminatedWordWrite guibg=#%s', vim.g.base16_gui02))

-- which-key
vim.o.timeoutlen = 500
require('which-key').setup({})

-- indent-blankline
require("ibl").setup()
vim.api.nvim_command(string.format('hi NonText guifg=#%s gui=nocombine', vim.g.base16_gui02))

-- gitsigns
require('gitsigns').setup({
  on_attach = function(buffer)
    local gs = package.loaded.gitsigns

    local function opts(desc)
      return { desc = 'Git: ' .. desc, buffer = buffer, noremap = true }
    end

    vim.keymap.set('n', '[c', function()
      if vim.wo.diff then return ']c' end
      vim.schedule(gs.prev_hunk)
      return '<Ignore>'
    end, opts('previous hunk'))

    vim.keymap.set('n', ']c', function()
      if vim.wo.diff then return ']c' end
      vim.schedule(gs.next_hunk)
      return '<Ignore>'
    end, opts('next hunk'))

    vim.keymap.set('n', '<Leader>gp', gs.preview_hunk, opts('preview hunk'))
    vim.keymap.set('n', '<Leader>gP', gs.preview_hunk_inline, opts('preview hunk inline'))
    vim.keymap.set('n', '<Leader>ga', gs.stage_hunk, opts('stage hunk'))
    vim.keymap.set('v', '<Leader>ga', ":'<,'>Gitsigns stage_hunk<CR>", opts('stage hunk'))
    vim.keymap.set('n', '<Leader>gu', gs.undo_stage_hunk, opts('undo stage hunk'))
    vim.keymap.set('v', '<Leader>gu', ":'<,'>Gitsigns undo_stage_hunk<CR>", opts('undo stage hunk'))
    vim.keymap.set('n', '<Leader>gr', gs.reset_hunk, opts('reset hunk'))
    vim.keymap.set('v', '<Leader>gr', ":'<,'>Gitsigns reset_hunk<CR>", opts('reset hunk'))
    vim.keymap.set('n', '<Leader>gb', gs.blame_line, opts('show blame for line'))
    vim.keymap.set('n', '<Leader>gd', gs.diffthis, opts('open diff for current file'))
    vim.keymap.set('n', '<Leader>gD', function() gs.diffthis('~') end,
      opts('open diff against HEAD for current file')
    )
  end
})

-- diffview
noremap('n', '<Leader>gdo', ':DiffviewOpen<CR>', { desc = 'Open git diff view' })
noremap('n', '<Leader>gdO', ':DiffviewOpen ', { desc = 'Start diff view command' })
noremap('n', '<Leader>gdc', ':DiffviewClose<CR>', { desc = 'Close git diff view' })
noremap('n', '<Leader>gdl', ':DiffviewFileHistory<CR>', { desc = 'Show git log for current file' })
noremap('n', '<Leader>gdf', ':DiffviewToggleFiles<CR>', { desc = 'Toggle file panel in diff view' })

-- dap-go
require('dap-go').setup()
noremap('n', '<Leader>dc', function() require('dap').continue() end, { desc = "Start or continue debugging session" })
noremap('n', '<Leader>do', function() require('dap').step_over() end, { desc = 'Step over' })
noremap('n', '<Leader>di', function() require('dap').step_into() end, { desc = 'Step into' })
noremap('n', '<Leader>du', function() require('dap').step_out() end, { desc = 'Step out' })
noremap('n', '<Leader>db', function() require('dap').toggle_breakpoint() end, { desc = 'Toggle breakpoint' })
noremap('n', '<Leader>dp', function()
  require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: '))
end, { desc = 'Log point message' })
noremap('n', '<Leader>do', function() require('dap').repl.open() end, { desc = 'Open REPL' })
noremap('n', '<Leader>dr', function() require('dap').run_last() end, { desc = 'Run last' })
noremap('n', '<Leader>dk', function() require('dap.ui.widgets').hover() end, { desc = 'dap-ui: Hover' })
noremap('n', '<Leader>dwp', function() require('dap.ui.widgets').preview() end, { desc = 'dap-ui: Preview' })
noremap('n', '<Leader>dwf', function()
  local widgets = require('dap.ui.widgets')
  widgets.centered_float(widgets.frames)
end, { desc = 'dap-ui: Frames' })
noremap('n', '<Leader>dws', function()
  local widgets = require('dap.ui.widgets')
  widgets.centered_float(widgets.scopes)
end, { desc = 'dap-ui: Scopes' })

-- nvim-dap-ui
require('dapui').setup()
noremap('n', '<Leader>dd', function() require("dapui").toggle() end, { desc = 'dap-ui: Toggle' })

-- conform.nvim
require('conform').setup({
  notify_on_error = false,
  formatters_by_ft = {
    terraform = { "tofu_fmt" },
  },
  format_on_save = {
    lsp_format = "fallback",
  },
})

-- nvim-lint
require('lint').linters_by_ft = {
  nix = {
    'deadnix',
    'statix',
  }
}
