local map = vim.keymap.set

require('telescope').setup {
  defaults = {
    disable_devicons = true,
    prompt_prefix = "",
    entry_prefix = "",
    selection_caret = "",
    layout_config = {
      preview_width = 0.625
    },
  },
  pickers = {
    find_files = {
      disable_devicons = true
    },
    git_files = {
      disable_devicons = true
    },
    live_grep = {
      disable_devicons = true
    },
  },
}

local builtin = require 'telescope.builtin'
map('n', '<C-p>', builtin.git_files)
map('n', '<leader>pf', builtin.find_files)
map('n', '<leader>rg', builtin.live_grep)
