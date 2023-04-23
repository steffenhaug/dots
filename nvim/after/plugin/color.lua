local Color, colors, Group, groups, styles = require('colorbuddy').setup()

vim.opt.winblend = 0
vim.opt.pumblend = 0

require("vitesse").setup {
  comment_italics = true,
  transparnet_background = true,
  transparent_float_background = true, -- aka pum(popup menu) background
  reverse_visual = false,
  dim_nc = false,
  cmp_cmdline_disable_search_highlight_group = false, -- disable search highlight group for cmp item
}

vim.cmd [[colorscheme vitesse]]

-- Hacking colorbuddy themes:
-- Use `get_captures_at_cursor` to figure out what group
-- something is.
--   vim.inspect(vim.treesitter.get_captures_at_cursor())

Color.new('green2', "#65A66B")
Color.new('yellow2', "#debd52")
Color.new('offwhite', "#ded6bd")
Group.new('@punctuation.bracket', colors.offwhite)
Group.new('@punctuation.special', groups['@punctuation.bracket'])
Group.new('@punctuation.delimiter', groups['@punctuation.bracket'])
Group.new('@string', colors.green)
Group.new('@number', colors.green)
Group.new('@float', colors.green)
Group.new('@namespace', colors.yellow2)
Group.new('@type', colors.green2)
Group.new('@type.builtin', groups['@type'])
