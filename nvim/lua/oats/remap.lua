local map = vim.keymap.set
vim.g.mapleader = ','

-- Hide search.
map('n', '<leader><esc>', ':noh<CR>')

-- Easy navigation between splits.
map('n', '<C-h>', '<C-w>h')
map('n', '<C-j>', '<C-w>j')
map('n', '<C-k>', '<C-w>k')
map('n', '<C-l>', '<C-w>l')

-- Open netrw
map('n', '<leader>pw', vim.cmd.Ex)
