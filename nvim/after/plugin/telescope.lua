local map = vim.keymap.set

require('telescope').setup {
  defaults = {
    prompt_prefix = "",
    entry_prefix = "",
    selection_caret = "",
    layout_config = {
      preview_width = 0.625
    },
    borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' }  }
}

local builtin = require 'telescope.builtin'
map('n', '<C-p>', builtin.git_files)
map('n', '<leader>pf', builtin.find_files)
map('n', '<leader>rg', builtin.live_grep)
