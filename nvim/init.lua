map = vim.keymap.set

require("oaty")

-- -- Undo-tree visualization.
-- 'mbbill/undotree';

vim.g.slime_no_mappings = 1

-- vim.opt.background = 'dark'
-- vim.cmd [[colorscheme gruvbox]]
vim.opt.winblend = 0
vim.opt.pumblend = 0
require("vitesse").setup {
  comment_italics = true,
  transparnet_background = true,
  transparent_float_background = true, -- aka pum(popup menu) background
  reverse_visual = false,
  dim_nc = false,
  cmp_cmdline_disable_search_highlight_group = false, -- disable search highlight group for cmp item
}
local Color, colors, Group, groups, styles = require('colorbuddy').setup()

-- Hacking colorbuddy themes:
-- Use `get_captures_at_cursor` to figure out what group
-- something is.
--   vim.inspect(vim.treesitter.get_captures_at_cursor())
Color.new('green2', "#65A66B")
Color.new('yellow2', "#debd52")
Color.new('offwhite', "#ded6bd")
Group.new('@punctuation.bracket', colors.offwhite)
Group.new('@punctuation.special', groups['@punctuation.bracket'])
Group.new('@punctuation.delimiter', groups['@punctuation.bracket'])
Group.new('@string', colors.green)
Group.new('@number', colors.green)
Group.new('@float', colors.green)
Group.new('@namespace', colors.yellow2)
Group.new('@type', colors.green2)
Group.new('@type.builtin', groups['@type'])

require('lualine').setup {
  options = {
    icons_enabled = false,
    theme = 'auto',
    component_separators = '',
    section_separators = '',
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {}
}
vim.cmd [[set noshowmode]]
vim.cmd [[set laststatus=3]]

require 'telescope' .setup {
  defaults = {
    prompt_prefix = "",
    entry_prefix = "",
    selection_caret = "",
    layout_config = {
      preview_width = 0.625
    },
    borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' }  }
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
    "sql",
    "julia",
    "latex"
  },
  highlight = { enable = true },
  indent = { enable = true },
}

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
