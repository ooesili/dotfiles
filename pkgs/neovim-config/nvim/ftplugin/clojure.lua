local opts = { noremap = true }
vim.keymap.set('n', '<Leader><Enter>', ':Eval<CR>', opts)
vim.keymap.set('n', '<Leader>r', ':Require<CR>', opts)

vim.g.clojure_align_subforms = 1

-- This option is global, so we need to figure another way of achieving this
-- vim.api.nvim_command('setlocal backspace-=indent')
