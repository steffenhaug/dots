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

local scope = require 'telescope.builtin'

map('n', '<leader>f', scope.git_files)  -- ,f for "find"
map('n', '<leader>rg', scope.live_grep) -- ,rg for ripgrep.
map('n', '<leader>h', scope.help_tags)  -- VIM help files.

