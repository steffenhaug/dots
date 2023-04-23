local map = vim.keymap.set

vim.g.slime_no_mappings = 1

vim.g.slime_target = "kitty"
vim.g.slime_cell_delimiter = "^\\s*##"
vim.g.slime_dont_ask_default = 1
vim.g.slime_bracketed_paste = 1
map('n', '<leader>cv', '<Plug>SlimeConfig')

-- In NeoVim, for some reason, the SlimeCells commands are not available,
-- however, the functions are available to be called directly.
map('n', '<leader>x',  vim.fn['slime_cells#send_cell'])
map('n', '<C-c><C-c>', vim.fn['slime_cells#send_cell'])
map('n', 'cj', vim.fn['slime_cells#go_to_next_cell'])
map('n', 'ck', vim.fn['slime_cells#go_to_previous_cell'])
