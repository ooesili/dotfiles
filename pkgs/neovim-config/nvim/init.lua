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
noremap('n', '<C-e>', '5<C-e>')
noremap('n', '<C-y>', '5<C-y>')
noremap('n', 'g<C-e>', '<C-e>')
noremap('n', 'g<C-y>', '<C-y>')
noremap('n', 'g<C-l>', '<C-l>')
noremap('n', '<Leader>w', ':update<CR>')
noremap('n', '<Leader>c', ':botright copen<CR>')
noremap('n', '<Leader>a', ':ALEResetBuffer<CR>')
noremap('n', '<Leader>to', ':NvimTreeOpen<CR>')
noremap('n', '<Leader>tc', ':NvimTreeClose<CR>')
noremap('n', '<Leader>tf', ':NvimTreeFindFile<CR>')
noremap('n', '<Leader>tF', ':NvimTreeFindFile!<CR>')
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
  options = { theme = 'base16-default-dark' }
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

-- ale
vim.g.ale_lint_on_text_changed = 'never'
vim.g.ale_linters = {
  c = {},
  go = {},
  ruby = { 'ruby' },
  javascript = {},
  lua = {},
  nix = {},
  typescript = {},
  rust = {},
  vue = {},
  zig = {}
}

-- direnv
augroup('InitVimDirenv', {
  { 'BufWritePost', '.envrc', 'silent', '!direnv allow %' }
})

-- terraform
vim.g.terraform_fmt_on_save = 1

-- trouble.nvim
require('trouble').setup({})

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
  skip_confirm_for_simple_edits = true
})

-- luasnip
local luasnip = require('luasnip')
require('luasnip.loaders.from_vscode').lazy_load()

-- nvim-cmp
local cmp = require('cmp')
cmp.setup({
  enabled = function()
    -- keep command mode completion enabled when cursor is in a comment
    if vim.api.nvim_get_mode().mode == 'c' then
      return true
    end

    -- disable completion when using telescope
    if vim.api.nvim_get_option_value('filetype', {}) == 'TelescopePrompt' then
      return false
    end

    -- disable completion in comments
    local context = require('cmp.config.context')
    return not context.in_treesitter_capture("comment")
        and not context.in_syntax_group("Comment")
  end,
  preselect = cmp.PreselectMode.None,
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-y>'] = cmp.mapping.scroll_docs(-3),
    ['<C-e>'] = cmp.mapping.scroll_docs(3),
    ['<C-Space>'] = cmp.mapping.complete(),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.get_selected_entry() then
        cmp.confirm()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }), -- {'i','s','c'}
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<C-f>'] = cmp.mapping.complete({
      config = {
        sources = { { name = "path" } },
      },
    }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'nvim_lsp_signature_help' },
    { name = 'luasnip' },
  }, {
    { name = 'buffer' },
  })
})
cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

require('neodev').setup({
  override = function(root_dir, library)
    if require("neodev.util").has_file(root_dir, "/nvim/") then
      library.enabled = true
      library.plugins = true
    end
  end,
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
    vim.keymap.set('n', '[d', ':lua vim.diagnostic.goto_prev()<CR>', opts)
    vim.keymap.set('n', ']d', ':lua vim.diagnostic.goto_next()<CR>', opts)
    vim.keymap.set('n', 'K', ':lua vim.lsp.buf.hover()<CR>', opts)
    vim.keymap.set('n', '<Leader>ln', ':lua vim.lsp.buf.rename()<CR>', opts)
    vim.keymap.set('n', '<Leader>la', ':lua vim.lsp.buf.code_action()<CR>', opts)
    vim.keymap.set('v', '<Leader>la', ':lua vim.lsp.buf.range_code_action()<CR>', opts)
    vim.keymap.set('n', '<Leader>lr', ':Telescope lsp_references theme=cursor<CR>', opts)
    vim.keymap.set('n', '<Leader>ls', ':SymbolsOutline<CR>', opts)
    vim.keymap.set('n', '<Leader>ld', ':Telescope diagnostics<CR>', opts)
    vim.keymap.set('n', '<Leader>lt', ':Telescope lsp_type_definitions<CR>', opts)
  end
})

local lspconfig = require('lspconfig')
local cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()

local lsp_servers = {
  bashls = {},
  gopls = {},
  lua_ls = {},
  nil_ls = {},
  pyright = {},
  rust_analyzer = {},
  tsserver = {},
  zls = {},
}
for name, config in pairs(lsp_servers) do
  config.capabilities = cmp_capabilities
  lspconfig[name].setup(config)
end


-- language specific settings
lspconfig.bashls.setup({})
lspconfig.gopls.setup({})
lspconfig.lua_ls.setup({})
lspconfig.nil_ls.setup({})
lspconfig.pyright.setup({})
lspconfig.rust_analyzer.setup({})
lspconfig.tsserver.setup({})
lspconfig.zls.setup({})

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
require('nvim-treesitter.configs').setup {
  highlight = { enable = true },
  incremental_selection = { enable = true },
  textobjects = { enable = true },

  -- dont' run regex parsing at the same time
  additional_vim_regex_highlighting = false,
}
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
    ['*'] = { 'rclip', 'copy', '--primary' },
    ['+'] = { 'rclip', 'copy', '--clipboard' }
  },
  paste = {
    ['*'] = { 'rclip', 'paste', '--primary' },
    ['+'] = { 'rclip', 'paste', '--clipboard' }
  }
}

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
noremap('n', '<Leader>do', ':DiffviewOpen<CR>', { desc = 'Open git diff view' })
noremap('n', '<Leader>dO', ':DiffviewOpen ', { desc = 'Start diff view command' })
noremap('n', '<Leader>dc', ':DiffviewClose<CR>', { desc = 'Close git diff view' })
noremap('n', '<Leader>dl', ':DiffviewFileHistory<CR>', { desc = 'Show git log for current file' })
noremap('n', '<Leader>df', ':DiffviewToggleFiles<CR>', { desc = 'Toggle file panel in diff view' })

-- null-ls
local null_ls = require('null-ls')
null_ls.setup({
  sources = {
    -- TODO add eslint
    null_ls.builtins.code_actions.statix,
    null_ls.builtins.diagnostics.deadnix,
    null_ls.builtins.diagnostics.statix,
    null_ls.builtins.formatting.alejandra,
    null_ls.builtins.formatting.jq,
  }
})

vim.filetype.add({
  extension = {
    -- zellij scrollback editor
    dump = function(path)
      local re = vim.regex('/[0-9a-f]\\{8}-[0-9a-f]\\{4}-[0-9a-f]\\{4}-[0-9a-f]\\{4}-[0-9a-f]\\{12}\\.dump$')
      if not re:match_str(path) then
        return
      end
      return 'zellij-dump', function()
        vim.wo[0].number = false
        vim.api.nvim_command('DisableWhitespace')
      end
    end
  }
})
