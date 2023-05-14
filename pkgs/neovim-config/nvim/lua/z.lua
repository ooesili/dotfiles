local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local finders = require('telescope.finders')
local pickers = require('telescope.pickers')
local conf = require('telescope.config').values

local z = {}

function z.telescope(opts)
  opts = opts or {}
  pickers.new(opts, {
    prompt_title = "Recent Directories",
    finder = finders.new_oneshot_job({ 'zdirs' }, opts),
    sorter = conf.file_sorter(opts),
    attach_mappings = function(prompt_bufnr)
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local selection = action_state.get_selected_entry()
        vim.api.nvim_command('lcd ' .. selection[1])
      end)
      return true
    end
  }):find()
end

return z
