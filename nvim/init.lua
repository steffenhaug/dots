map = vim.keymap.set
require("oats")

-- Language server protocol.
-- =========================

-- See `:help vim.diagnostic.*` for documentation on any of the below functions local opts = { noremap=true, silent=true }
map('n', '<leader>e', vim.diagnostic.open_float, opts)
map('n', '[d', vim.diagnostic.goto_prev, opts)
map('n', ']d', vim.diagnostic.goto_next, opts)
map('n', '<leader>q', vim.diagnostic.setloclist, opts)

local lsp_attach = function(client, bufn)
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

  -- Makes gq not use the LSP formatter
  vim.opt.formatexpr = ""

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

-- Configure nvim-cmp
local cmp = require "cmp"
local luasnip = require "luasnip"
vim.opt.completeopt = {'menu', 'menuone', 'noselect'}

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  sources = {
    { name = 'nvim_lsp', keyword_length = 3 },
    { name = 'nvim_lua', keyword_length = 3 },
    { name = 'buffer', keyword_length = 3 },
    { name = 'path', keyword_length = 2 },
  },
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
    ['<C-n>'] = cmp.mapping.select_next_item(select_opts),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<C-y>'] = cmp.mapping.confirm {
      select = true,
    },
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
  settings = {
    haskell = {
        formattingProvider = 'stylish-haskell'
    }
  }
}

require 'lspconfig'.julials.setup {
  on_attach = lspconfig,
  capabilities = capabilities,
  flags = lsp_flags
}

-- Open floating diagnostics when holding the cursor still.
-- open_float
vim.opt.updatetime = 300
vim.cmd [[ autocmd CursorHold * lua vim.diagnostic.open_float({focusable = false}) ]]

require 'fidget' .setup {
  text = {
    spinner = "dots"
  }
}
