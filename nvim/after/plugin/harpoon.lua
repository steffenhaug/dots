local map = vim.keymap.set
local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

map("n", "<leader>a", mark.add_file)
map("n", "<C-e>", ui.toggle_quick_menu)

map("n", "<C-j>", function() ui.nav_file(1) end)
map("n", "<C-k>", function() ui.nav_file(2) end)
map("n", "<C-l>", function() ui.nav_file(3) end)
