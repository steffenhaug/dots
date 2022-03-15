-- Contains a callback to give to lsp-analyzer-setups to
-- set up settings that are only relevant after attaching
-- to a language server.
require 'ticket-to-the-moon'

local lsp_attach = function(client, bufn)
    -- Load plugins that require LSP client.
    require "completion" .on_attach(client)

    -- Change formatting of diagnostic messages.
    vim.lsp.handlers["textDocument/publishDiagnostics"] = 
        vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
            -- Finally a good use for font ligatures! :-)
            virtual_text = { spacing = 2, prefix = "<!--" },
            severity = {min=vim.diagnostic.severity.WARN}
        })

    -- Less intrusive autocompletion.
    vim.opt.completeopt = { "menuone", "noinsert", "noselect" }

    vim.g.completion_matching_strategy_list = { "exact", "fuzzy" }
    vim.g.completion_enable_auto_popup = 0

    -- Keybindings for lsp actions.
    nnoremap { 'g[',  function() vim.diagnostic.goto_prev {enable_popup = false} end }
    nnoremap { 'g]',  function() vim.diagnostic.goto_next {enable_popup = false} end }
    nnoremap { 'gd',  vim.lsp.buf.definition }
    nnoremap { 'gi',  vim.lsp.buf.implementation }
    nnoremap { 'K',   vim.lsp.buf.hover }
    nnoremap { '<leader>p', vim.lsp.buf.formatting }
end

return lsp_attach
