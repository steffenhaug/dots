local map = vim.keymap.set
local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

map("n", "<leader>a", mark.add_file)
map("n", "<C-e>", ui.toggle_quick_menu)

map("n", "<C-m>", function() ui.nav_file(1) end)
map("n", "<C-,>", function() ui.nav_file(2) end)
map("n", "<C-.>", function() ui.nav_file(3) end)
map("n", "<C-/>", function() ui.nav_file(4) end)
