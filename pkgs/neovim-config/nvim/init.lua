-- luacheck: globals vim

local function noremap(mode, lhs, rhs)
  vim.api.nvim_set_keymap(mode, lhs, rhs, {noremap = true})
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
vim.api.nvim_command('colorscheme base16-default-dark')

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

-- ripgrep
if vim.fn.executable('rg') == 1 then
  vim.o.grepprg = 'rg --vimgrep --no-heading'
  vim.o.grepformat = '%f:%l:%c:%m,%f:%l:%m'
end

-- yank and paste to system clipboard
vim.o.clipboard = 'unnamedplus'

-- mappings
vim.g.mapleader = ' '
noremap('n', '<Leader>e', ':Explore<CR>')
noremap('n', '<Leader>w', ':update<CR>')
noremap('n', '<Leader>c', ':botright copen<CR>')
noremap('n', '<Leader>a', ':ALEResetBuffer<CR>')
noremap('n', '<C-e>', '5<C-e>')
noremap('n', '<C-y>', '5<C-y>')
noremap('n', 'g<C-e>', '<C-e>')
noremap('n', 'g<C-y>', '<C-y>')
noremap('n', 'g<C-l>', '<C-l>')
-- window navigation
noremap('n', '<C-h>', '<C-w><C-h>')
noremap('n', '<C-j>', '<C-w><C-j>')
noremap('n', '<C-k>', '<C-w><C-k>')
noremap('n', '<C-l>', '<C-w><C-l>')

-- snippets
vim.g.UltiSnipsEditSplit = 'context'

-- status line
vim.g.airline_theme = 'base16'
vim.g['airline#extensions#tmuxline#enabled'] = 0

-- terminal
noremap('t', '<C-l>', '<nop>')
vim.o.scrollback = 10000
augroup('InitVimTerminalSettings', {
  {'TermOpen', '*', 'setlocal', 'nonumber'}
})

-- extra filetypes
augroup('InitVimFiletypes', {
  {'BufNewFile,BufRead', '.envrc', 'set', 'filetype=sh'},
  {'BufNewFile,BufRead', 'Vagrantfile,Berksfile', 'set', 'filetype=ruby'},
  {'BufNewFile,BufRead', 'tmux.conf', 'set', 'filetype=tmux.conf'},
  {'BufNewFile,BufRead', '*.hcl', 'set', 'filetype=terraform'},
  {'BufNewFile,BufRead', '*.tfstate', 'setlocal', 'filetype=json shiftwidth=4 softtabstop=4'},
  {'BufNewFile,BufRead', '*.tsx', 'set', 'filetype=typescriptreact'}
})

-- vim-commentary extensions
augroup('InitVimComentary', {
  {'FileType', 'tf', 'setlocal', 'commentstring=#\\ %s'},
  {'FileType', 'sql', 'setlocal', 'commentstring=--\\ %s'}
})

-- ale
vim.g.ale_lint_on_text_changed = 'never'
vim.g.ale_linters = {
  c = {},
  go = {},
  ruby = {'ruby'},
  javascript = {},
  typescript = {},
  rust = {},
  zig = {}
}

-- direnv
augroup('InitVimDirenv', {
  {'BufWritePost', '.envrc', 'silent', '!direnv allow %'}
})

-- terraform
vim.g.terraform_fmt_on_save = 1

-- telescope
local telescope_actions = require('telescope.actions')
local telescope = require('telescope')
telescope.setup({
  defaults = {
    layout_strategy = 'flex',
    mappings = {
      i = {
        -- clear input instead of scrolling
        ["<C-u>"] = false,
        ["<Esc>"] = telescope_actions.close,
        ["<C-c>"] = function()
          vim.cmd('stopinsert')
        end
      }
    }
  },
  extensions = {
    fzf = {},
    ["ui-select"] = {
      require("telescope.themes").get_cursor {}
    }
  }
})
telescope.load_extension('fzf')
telescope.load_extension('ui-select')
noremap('n', '<Leader>f', ':Telescope find_files<CR>')
noremap('n', '<Leader>b', ':Telescope buffers<CR>')
noremap('n', '<Leader>o', ':Telescope oldfiles<CR>')
noremap('n', '<Leader>g', ':Telescope live_grep<CR>')
noremap('n', '<Leader>z', ':lua require("z").telescope()<CR>')
noremap('n', 'z=', ':Telescope spell_suggest<CR>')

-- nvim-cmp
local cmp = require('cmp')
local cmp_lsp_capabilities = require('cmp_nvim_lsp').default_capabilities
cmp.setup({
  mapping = cmp.mapping.preset.insert({
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
  }),

  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'nvim_lsp_signature_help' },
  }, {
    { name = 'buffer' },
  })
})
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- lsp
local lsp_on_attach = function(_, bufnr)
  local opts = { noremap=true, silent=true }

  -- Format on save
  vim.api.nvim_command('autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()')

  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', ':Telescope lsp_definitions theme=cursor<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', ':lua vim.diagnostic.goto_prev()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', ':lua vim.diagnostic.goto_next()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', ':lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>ln', ':lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>la', ':lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'v', '<Leader>la', ':lua vim.lsp.buf.range_code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>lr', ':Telescope lsp_references theme=cursor<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>ls', ':Telescope lsp_workspace_symbols<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>ld', ':Telescope diagnostics<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>lt', ':Telescope lsp_type_definitions<CR>', opts)
end

local lspconfig = require('lspconfig')
local setup_config = {
  on_attach = lsp_on_attach,
  flags = {
    -- This will be the default in neovim 0.7+
    debounce_text_changes = 150,
  },
  capabilities = cmp_lsp_capabilities,
}
lspconfig.bashls.setup(setup_config)
lspconfig.gopls.setup(setup_config)
lspconfig.rust_analyzer.setup(setup_config)
lspconfig.tsserver.setup(setup_config)
lspconfig.zls.setup(setup_config)

-- better whitespace
vim.g.better_whitespace_filetypes_blacklist = {
  'diff', 'gitcommit', 'unite', 'qf', 'help', 'git'
}
vim.api.nvim_command('autocmd FileType fugitive DisableWhitespace')

-- change into the directory of the current file
vim.api.nvim_command('command! CD lua require("mapfuncs").cd()')

-- treesitter
require('nvim-treesitter.configs').setup {
  -- Modules and its options go here
  highlight = { enable = true },
  incremental_selection = { enable = true },
  textobjects = { enable = true },
}
require('treesitter-context').setup({
  separator = '-',
})
vim.o.foldmethod = 'expr'
vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
vim.o.foldenable = false

-- nix
vim.api.nvim_command('autocmd BufRead,BufNewFile flake.lock setfiletype json')

-- fugitive
vim.g.fugitive_pty = 0

-- rclip
vim.g.clipboard = {
  name = 'rclip',
  copy = {
    ['*'] = {"rclip", "copy", "--primary"},
    ['+'] = {"rclip", "copy", "--clipboard"}
  },
  paste = {
    ['*'] = {"rclip", "paste", "--primary"},
    ['+'] = {"rclip", "paste", "--clipboard"}
  }
}
