require 'packer'.startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- Post-install/update hook with neovim command
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use { 'nvim-treesitter/playground' }

  -- Telescope
  use {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.0',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  use {
    'ThePrimeagen/harpoon',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- Modeline
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'nvim-tree/nvim-web-devicons', opt = true }
  }

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

  -- Completion: nvim-cmp requires a snippet engine, I use LuaSnip.
  use 'L3MON4D3/LuaSnip'
  use "hrsh7th/nvim-cmp"

  -- Completion sources.
  use "hrsh7th/cmp-nvim-lua"
  use "hrsh7th/cmp-nvim-lsp"  
  use "hrsh7th/cmp-buffer"
  use "hrsh7th/cmp-path"

  -- Configurations for Nvim LSP
  use 'neovim/nvim-lspconfig'

  -- Loading bar for LSP
  use 'j-hui/fidget.nvim'

end)
