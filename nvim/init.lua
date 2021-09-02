-- Aliases to set options and execute Vimscript commands.
local opt       = vim.opt
local vimscript = vim.cmd

-- Sensible TAB settings.
opt.tabstop    = 4
opt.shiftwidth = 4
opt.expandtab  = true

-- Line & cursor behaviour.
opt.number         = true   -- Show line numbers.
opt.wrap           = false  -- Disable line weapping.
opt.scrolloff      = 5      -- Reserve space past cursor vertically.
opt.sidescrolloff  = 15     -- Reserve space past cursor horizontally.

-- Interaction with wider system.
opt.clipboard     = "unnamedplus"   -- Yank to clipboard.
opt.termguicolors = true            -- Assume true color terminal.

-- Paq package manager. (https://github.com/savq/paq-nvim)
require "paq" {
    -- Paq manages itself.
    "savq/paq-nvim";
    -- Packages:
    "nvim-treesitter/nvim-treesitter";
    "neovim/nvim-lspconfig";
    "nvim-lua/completion-nvim";
}

-- Treesitter.
require "nvim-treesitter.configs" .setup {
    ensure_installed = { "rust", "lua", "c", "fish" },
    highlight        = { enable = true },
}

-- Language server protocol.
local nvlsp = require "lspconfig"
-- Callback to configure settings that are only relevant after
-- attaching to a language server.
local on_attach = function(client)
    -- Load plugins that require LSP client.
    require "completion" .on_attach(client)

    -- Change formatting of diagnostic messages.
    vim.lsp.handlers["textDocument/publishDiagnostics"] = 
        vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
            -- Finally a good use for font ligatures! :-)
            virtual_text = { spacing = 6, prefix = "<!--" }
        })

    -- Less intrusive autocompletion.
    opt.completeopt = { "menuone", "noinsert", "noselect" }

    -- Signs in the number column.
    opt.signcolumn  = "number"

    -- Vimscript block more or less copied exactly from the 
    -- suggested lsp-settings for autocomplete behaviour.
    vimscript [[
    " Tab behaviour.
    inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
    inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
    imap <Tab> <Plug>(completion_smart_tab)
    imap <S-Tab> <Plug>(completion_smart_s_tab)

    " Don't pop up automatically.
    let g:completion_enable_auto_popup = 0

    " Suggest fuzzy matches.
    let g:completion_matching_strategy_list = [ 'exact', 'fuzzy' ]
    ]]

    -- Keybindings for lsp actions.
    vimscript [[
    " Goto previous/next diagnostic warning/error.
    nnoremap <silent> g[ <cmd>lua vim.lsp.diagnostic.goto_prev { enable_popup = false }<CR>
    nnoremap <silent> g] <cmd>lua vim.lsp.diagnostic.goto_next { enable_popup = false }<CR>
    ]]
end

-- Installation as of 2021:
--  git clone
--  cargo xtask install --server
nvlsp.rust_analyzer.setup { on_attach = on_attach }

