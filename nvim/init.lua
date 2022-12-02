map = vim.keymap.set

-- Leader needs to be set before we start configuring plugins.
vim.g.mapleader = ','

require 'packer'.startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- Post-install/update hook with neovim command
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  -- Configurations for Nvim LSP
  use 'neovim/nvim-lspconfig'

  -- Loading bar for LSP
  use 'j-hui/fidget.nvim'

  -- Telescope
  use {
    'nvim-telescope/telescope.nvim',
      tag = '0.1.0',
      requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- Color scheme.
  use 'morhetz/gruvbox'

  -- NVIM in Firefox text boxes.
  use {
    "glacambre/firenvim",
    run = function()
      vim.fn["firenvim#install"](0)
    end,
  }

  -- Completion: nvim-cmp requires a snippet engine, I use LuaSnip.
  use 'L3MON4D3/LuaSnip'
  use "hrsh7th/nvim-cmp"

  -- Completion sources.
  use "hrsh7th/cmp-nvim-lua"
  use "hrsh7th/cmp-nvim-lsp"  
  use "hrsh7th/cmp-buffer"
  use "hrsh7th/cmp-path"
end)

-- -- Undo-tree visualization.
-- 'mbbill/undotree';

vim.opt.background = 'dark'
vim.cmd [[colorscheme gruvbox]]

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
            preview_width = 0.625
        },
        borderchars = single_border
    }
}

local scope = require 'telescope.builtin'

map('n', '<leader>f', scope.git_files)  -- ,f for "find"
map('n', '<leader>rg', scope.live_grep) -- ,rg for ripgrep.
map('n', '<leader>h', scope.help_tags)  -- VIM help files.

-- Treesitter grammars:
require "nvim-treesitter.configs" .setup {
    ensure_installed = {
        "haskell",
        "python",
        "rust",
        "lua",
        "c",
        "fish",
        "glsl",
        "sql"
    },
    highlight = { enable = true },
    indent = { enable = true },
}

 
-- Language server protocol.
-- =========================

-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
map('n', '<leader>e', vim.diagnostic.open_float, opts)
map('n', '[d', vim.diagnostic.goto_prev, opts)
map('n', ']d', vim.diagnostic.goto_next, opts)
map('n', '<leader>q', vim.diagnostic.setloclist, opts)
 
local lsp_attach = function(client, bufn)
    -- Callback for after connecting to the langauge server, so
    -- settings can apply only when we are actually connected to 
    -- a language server.

    -- Change formatting of diagnostic messages.
    vim.lsp.handlers["textDocument/publishDiagnostics"] = 
        vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
            -- Finally a good use for font ligatures! 8v)
            virtual_text = { 
                spacing = 2,
                prefix = "<!--"
            },
            -- Don't show info messages.
            severity = {
                min = vim.diagnostic.severity.WARN
            }
        })

    local bufopts = { noremap=true, silent=true, buffer=bufn }

    -- Keybindings for lsp actions.
    map('n', 'gd',  vim.lsp.buf.definition, bufopts)
    map('n', 'gi',  vim.lsp.buf.implementation, bufopts)
    map('n', 'K',   vim.lsp.buf.hover, bufopts)
    map('n', '<C-p>', function() vim.lsp.buf.format { async = true } end, bufopts)
    map('n', '<leader>ty', vim.lsp.buf.type_definition, bufopts)
    map('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
end

-- Rust-analyzer installation as of 2021:
--  * git clone
--  * cargo xtask install --server

-- Set up all the servers
-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#zls
lsp_flags = {
    debounce_text_changes = 150,
}

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
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm {
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
        { name = 'nvim_lua' },
        { name = 'path'},
        { name = 'buffer' },
    },
}

-- Set up lspconfig with extra capabilities from nvim-cmp.
local capabilities = require('cmp_nvim_lsp').default_capabilities()

require "lspconfig".rust_analyzer.setup {
    on_attach = lsp_attach,
    flags = lsp_flags,
    cababilities = capabilities,
    settings = {
        ["rust-analyzer"] = {
            diagnostics = { disabled = {"unresolved-proc-macro"} }
        }
    }
}

require "lspconfig".pyright.setup {
    on_attach = lsp_attach,
    cababilities = capabilities,
    flags = lsp_flags,
}

require "lspconfig".hls.setup {
    on_attach = lsp_attach,
    cababilities = capabilities,
    flags = lsp_flags,
}


-- Open floating diagnostics when holding the cursor still.
-- open_float
vim.opt.updatetime = 300
vim.cmd [[ autocmd CursorHold * lua vim.diagnostic.open_float({focusable = false}) ]]


-- Key Mappings.
-- =============

-- Hide search.
map('n', '<leader><esc>', ':noh<CR>')

-- Easy navigation between splits.
map('n', '<C-h>', '<C-w>h')
map('n', '<C-j>', '<C-w>j')
map('n', '<C-k>', '<C-w>k')
map('n', '<C-l>', '<C-w>l')

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

-- I don't think disabling these help, but it makes :checkhealth look clean.
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_node_provider = 0

require 'fidget' .setup {
  text = {
    spinner = "dots"
  }
}
