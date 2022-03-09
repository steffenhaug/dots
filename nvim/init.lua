-- Homemade crap.
require 'ticket-to-the-moon'

-- Leader needs to be set before we start configuring plugins.
vim.g.mapleader = ','

-- Paq package manager. (https://github.com/savq/paq-nvim)
require 'paq' {
    -- Paq manages itself.
    'savq/paq-nvim';
    -- Better syntax-highlighting and builtin LSP configuration.
    'nvim-treesitter/nvim-treesitter';
    'neovim/nvim-lspconfig';
    -- Autocomplete + LSP completion source.
    'L3MON4D3/LuaSnip';
    'hrsh7th/cmp-nvim-lsp';
    'hrsh7th/nvim-cmp';
    -- Coroutine library (telescope dependency).
    'nvim-lua/plenary.nvim';
    -- Icons (patched font, requires modifying kitty config).
    'kyazdani42/nvim-web-devicons';
    'yamatsum/nvim-nonicons';
    -- Telescope.
    'nvim-telescope/telescope.nvim';
    -- Color schemes.
    'morhetz/gruvbox';
}

-- Plugin-related configuration.
vim.opt.background = 'dark'
colorscheme 'gruvbox'

-- My telescope settings.
require 'st/telescope'

-- Treesitter.
require "nvim-treesitter.configs" .setup {
    ensure_installed = { "haskell", "python", "rust", "lua", "c", "fish" },
    highlight        = { enable = true },
}

-- Language server protocol.
-- =========================

-- Enable additional capabilities supported by nvim-cmp.
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

-- Rust-analyzer installation as of 2021:
--  * git clone
--  * cargo xtask install --server

-- Set up all the servers
local lsp_servers = { "rust_analyzer" }
for _, server in pairs(lsp_servers) do
    require("lspconfig")[server].setup {
        on_attach = require 'lsp-attach',
        -- Use additional capabilities.
        cababilities = capabilities,
        flags = {
            debounce_text_changes = 150,
        }
    }
end

-- Luasnip is used to parse LSP-style snippets.
local luasnip = require "luasnip"

-- Configure nvim-cmp
local cmp = require "cmp"
cmp.setup {
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end,
    },
    mapping = {
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<CR>'] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
        },
        ['<Tab>'] = function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            else
                fallback()
            end
        end,
        ['<S-Tab>'] = function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end,
    },
    sources = {
        -- Get autocompletions from LSP.
        { name = 'nvim_lsp' },
    }
}

-- Open floating diagnostics when holding the cursor still.
-- open_float
vim.opt.updatetime = 300
vim.cmd [[ autocmd CursorHold * lua vim.diagnostic.open_float() ]]


-- Key Mappings.
-- =============

-- Hide search.
nnoremap { '<leader><esc>', ':noh<CR>', silent=true }

-- Easy navigation between splits.
nnoremap { '<C-h>', '<C-w>h', silent=true }
nnoremap { '<C-j>', '<C-w>j', silent=true }
nnoremap { '<C-k>', '<C-w>k', silent=true }
nnoremap { '<C-l>', '<C-w>l', silent=true }

-- Easy jump to start and end of line.
nnoremap { 'H', '^', silent=true }
nnoremap { 'L', '$', silent=true }

-- Sensible TAB settings.
vim.opt.tabstop    = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab  = true

-- Line & cursor behaviour.
vim.opt.number        = true            -- Show (absolute) line numbers.
vim.opt.wrap          = false           -- Disable line wrapping.
vim.opt.scrolloff     = 5               -- Reserve space past cursor vertically.
vim.opt.sidescrolloff = 15              -- Reserve space past cursor horizontally.

-- Interaction with wider system.
vim.opt.clipboard     = "unnamedplus"   -- Yank to clipboard.
vim.opt.termguicolors = true            -- Assume true color terminal.

-- Look and feel.
vim.opt.signcolumn = "number"           -- Signs in the number column.

-- Persistent undo. (NVIM has a good default location so no need to change)
vim.opt.undofile = true
