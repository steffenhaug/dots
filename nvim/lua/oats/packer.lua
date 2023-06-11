require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use { 'nvim-treesitter/playground' }

  use {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.0',
    requires = { { 'nvim-lua/plenary.nvim' } }
  }

  use { 'mbbill/undotree' }

  use {
    'ThePrimeagen/harpoon',
    requires = { { 'nvim-lua/plenary.nvim' } }
  }

  use { 'nvim-lualine/lualine.nvim' }

  -- Color schemes.
  use 'morhetz/gruvbox'
  use 'tjdevries/colorbuddy.nvim'
  use {
    '2nthony/vitesse.nvim',
    requires = { 'tjdevries/colorbuddy.nvim' }
  }

  -- Interact with REPLs.
  use 'jpalardy/vim-slime'
  use {
    'klafyvel/vim-slime-cells',
    requires = { 'jpalardy/vim-slime' },
  }

  -- LaTeX input ++
  use { 'JuliaEditorSupport/julia-vim' }

  -- EZ LSP
  use {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    requires = {
      -- LSP Support
      { 'neovim/nvim-lspconfig' }, -- Required
      {
        -- Optional
        'williamboman/mason.nvim',
        run = function()
          pcall(vim.cmd, 'MasonUpdate')
        end,
      },
      { 'williamboman/mason-lspconfig.nvim' }, -- Optional

      -- Autocompletion
      { 'hrsh7th/nvim-cmp' },     -- Required
      { 'hrsh7th/cmp-nvim-lsp' }, -- Required
      { 'L3MON4D3/LuaSnip' },     -- Required

      -- Loading widget to show wtf is taking so long
      { 'j-hui/fidget.nvim' },
    }
  }
end)
