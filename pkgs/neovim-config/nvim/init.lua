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

-- disable netrw as recommended by nvim-tree.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

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
-- telescope
noremap('n', '<Leader>f', ':Telescope find_files<CR>')
noremap('n', '<Leader>b', ':Telescope buffers<CR>')
noremap('n', '<Leader>o', ':Telescope oldfiles<CR>')
noremap('n', '<Leader>s', ':Telescope live_grep<CR>')
noremap('n', '<Leader>z', require("z").telescope, { desc = "Quick change directory" })
noremap('n', 'z=', ':Telescope spell_suggest<CR>')
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
	{ 'BufNewFile,BufRead', '.envrc', 'set', 'filetype=sh' },
	{ 'BufNewFile,BufRead', 'Vagrantfile,Berksfile', 'set', 'filetype=ruby' },
	{ 'BufNewFile,BufRead', 'tmux.conf', 'set', 'filetype=tmux.conf' },
	{ 'BufNewFile,BufRead', '*.hcl', 'set', 'filetype=terraform' },
	{ 'BufNewFile,BufRead', '*.tfstate', 'setlocal', 'filetype=json shiftwidth=4 softtabstop=4' },
	{ 'BufNewFile,BufRead', '*.tsx', 'set', 'filetype=typescriptreact' }
})

-- vim-commentary extensions
augroup('InitVimComentary', {
	{ 'FileType', 'tf', 'setlocal', 'commentstring=#\\ %s' },
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
	typescript = {},
	rust = {},
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

-- nvim-tree.lua
require('nvim-tree').setup({
	view = {
		mappings = {
			list = {
				{ key = '<C-k>', action = '' },
				{ key = 'i', action = 'toggle_file_info' },
			}
		}
	},
	renderer = { indent_markers = { enable = true } }
})
vim.api.nvim_command(string.format('hi NvimTreeIndentMarker guifg=#%s', vim.g.base16_gui02))

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
		if vim.api.nvim_buf_get_option(0, 'filetype') == 'TelescopePrompt' then
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
local lsp_on_attach = function(_, bufnr)
	local opts = { noremap = true, silent = true }

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
	vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>ls', ':SymbolsOutline<CR>', opts)
	vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>ld', ':Telescope diagnostics<CR>', opts)
	vim.api.nvim_buf_set_keymap(bufnr, 'n', '<Leader>lt', ':Telescope lsp_type_definitions<CR>', opts)
end

local lspconfig = require('lspconfig')
local cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()
-- defaults
lspconfig.util.default_config.on_attach = lsp_on_attach
lspconfig.util.default_config.capabilities = cmp_capabilities
-- language specific settings
lspconfig.bashls.setup({})
lspconfig.gopls.setup({})
lspconfig.rust_analyzer.setup({})
lspconfig.sumneko_lua.setup({})
lspconfig.tsserver.setup({})
lspconfig.zls.setup({})

-- better whitespace
vim.g.better_whitespace_filetypes_blacklist = {
	'diff', 'gitcommit', 'unite', 'qf', 'help', 'git'
}
vim.api.nvim_command('autocmd FileType fugitive DisableWhitespace')

-- change into the directory of the current file
vim.api.nvim_command('command! CD lua require("mapfuncs").cd()')

-- treesitter
require('nvim-treesitter.configs').setup {
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
require("indent_blankline").setup()
vim.api.nvim_command(string.format('hi IndentBlanklineChar guifg=#%s gui=nocombine', vim.g.base16_gui02))

-- gitsigns
require('gitsigns').setup({
	on_attach = function()
		local gs = package.loaded.gitsigns

		noremap('n', '[c', function()
			if vim.wo.diff then return ']c' end
			vim.schedule(gs.prev_hunk)
			return '<Ignore>'
		end)

		noremap('n', ']c', function()
			if vim.wo.diff then return ']c' end
			vim.schedule(gs.next_hunk)
			return '<Ignore>'
		end)

		noremap('n', '<Leader>gp', gs.preview_hunk, { desc = 'Git: preview hunk' })
		noremap('n', '<Leader>ga', gs.stage_hunk, { desc = 'Git: stage hunk' })
		noremap('n', '<Leader>gu', gs.undo_stage_hunk, { desc = 'Git: reset hunk' })
		noremap('n', '<Leader>gb', gs.blame_line, { desc = 'Git: show blame for line' })
		noremap('n', '<Leader>gd', gs.diffthis, { desc = 'Git: open diff for current file' })
		noremap('n', '<Leader>gD', function() gs.diffthis('~') end,
			{ desc = 'Git: open diff against HEAD for current file' }
		)
	end
})

-- symbols-outline.nvim
require('symbols-outline').setup({
	keymaps = {
		focus_location = '<Tab>',
		unfold = 'zo',
		fold = 'zc',
		unfold_all = 'zr',
		fold_all = 'zm',
		fold_reset = 'zi'
	}
})

-- null-ls
local null_ls = require('null-ls')
null_ls.setup({
  on_attach = lsp_on_attach,
  sources = {
    null_ls.builtins.code_actions.eslint,
    null_ls.builtins.code_actions.statix,
    null_ls.builtins.diagnostics.deadnix,
    null_ls.builtins.diagnostics.eslint,
    null_ls.builtins.diagnostics.statix,
    null_ls.builtins.formatting.alejandra,
    null_ls.builtins.formatting.eslint,
    null_ls.builtins.formatting.jq,
    null_ls.builtins.formatting.prettier,
    null_ls.builtins.formatting.terraform_fmt,
  }
})
