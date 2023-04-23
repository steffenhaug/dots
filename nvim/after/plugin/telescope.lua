require 'telescope' .setup {
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
map('n', '<leader>pf', builtin.live_grep)
