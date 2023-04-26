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

-- Some gruvbox background colors.
local bg0 = '#282828'
local bg0_h = '#1d2021'
local bg1 = '#3c3836'

-- Some custom colors
local offwhite = '#ded6bd'
local green1 = '#4d9375'
local green2 = "#65A66B"
local lightgreen = '#80a665'
local yellow2 = "#debd52"

Color.new('green2', green2)
Color.new('yellow2', yellow2)
Color.new('offwhite', offwhite)
Group.new('@punctuation.bracket', colors.offwhite)
Group.new('@punctuation.special', groups['@punctuation.bracket'])
Group.new('@punctuation.delimiter', groups['@punctuation.bracket'])
Group.new('@string', colors.green)
Group.new('@number', colors.green)
Group.new('@float', colors.green)
Group.new('@namespace', colors.yellow2)
Group.new('@type', colors.green2)
Group.new('@type.builtin', groups['@type'])

-- Highlights defined by Telescope:
-- https://github.com/nvim-telescope/telescope.nvim/blob/713d26b98583b160b50fb827adb751f768238ed3/plugin/telescope.lua#L11-L95
local TelescopePrompt = {
  TelescopePromptNormal = { fg = offwhite, bg = bg1, },
  TelescopePreviewNormal = { bg = bg0_h, },
  TelescopeResultsNormal = { fg = offwhite, bg = bg0, },
  TelescopePromptBorder = { fg = bg1, bg = bg1, },
  TelescopePreviewBorder = { fg = bg0_h, bg = bg0_h, },
  TelescopeResultsBorder = { fg = bg0, bg = bg0, },
  TelescopeSelection = { fg = lightgreen, bg = bg0 },
  TelescopeMatching = { fg = green1 },
  TelescopePromptTitle = { link = 'TelescopePromptBorder' },
  TelescopePreviewTitle = { link = 'TelescopePreviewBorder' },
  TelescopeResultsTitle = { link = 'TelescopeResultsBorder' },
}

for hl, col in pairs(TelescopePrompt) do
    vim.api.nvim_set_hl(0, hl, col)
end

