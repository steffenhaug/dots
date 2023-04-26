local map = vim.keymap.set
vim.g.mapleader = ','

-- Hide search.
map('n', '<leader><esc>', ':noh<CR>')

-- Open netrw
map('n', '<leader>pw', vim.cmd.Ex)
