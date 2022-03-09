require 'ticket-to-the-moon'

-- Some aliases to change the look of the boxes without 
-- trawling through unicode tables on Wikipedia.
local single_border = { '─', '│', '─', '│', '┌', '┐', '┘', '└' }
local double_border = { '═', '║', '═', '║', '╔', '╗', '╝', '╚' }

require 'telescope' .setup {
    defaults = {
        prompt_prefix = "",
        entry_prefix = "",
        selection_caret = "",
        layout_config = {
            preview_width = 0.65
        },
        borderchars = single_border
    }
}

local scope = require 'telescope.builtin'

nnoremap { '<leader>f', scope.git_files }
nnoremap { '<leader>g', scope.live_grep }
