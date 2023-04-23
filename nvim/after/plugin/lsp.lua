local lsp = require('lsp-zero').preset({})
local map = vim.keymap.set

lsp.ensure_installed({
  'rust_analyzer',
  'hls',
  'lua_ls',
  'julials',
})

-- Rust-analyzer installation as of 2021:
--  * git clone
--  * cargo xtask install --server

lsp.on_attach(function(client, bufn)
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

  local bufopts = { noremap=true, buffer=bufn }

  map('n', 'gd',  vim.lsp.buf.definition, bufopts)
  map('n', 'K',   vim.lsp.buf.hover, bufopts)
  map('n', '[d', vim.diagnostic.goto_prev, bufopts)
  map('n', ']d', vim.diagnostic.goto_next, bufopts)
end)

lsp.setup()

require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())

require('lspconfig').rust_analyzer.setup {
  settings = {
    ["rust-analyzer"] = {
      diagnostics = { disabled = {"unresolved-proc-macro"} }
    }
  }
}

require('lspconfig').hls.setup {
  settings = {
    haskell = {
        formattingProvider = 'stylish-haskell'
    }
  }
}

require('fidget').setup {
  text = {
    spinner = "dots"
  }
}
