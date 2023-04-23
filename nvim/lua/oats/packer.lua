
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

  use {
    'metalelf0/jellybeans-nvim',
    requires = { {'rktjmp/lush.nvim'}}
  }

  use {
    "2nthony/vitesse.nvim",
    requires = {
      "tjdevries/colorbuddy.nvim"
    }
  }

  -- Interact with REPLs.
  use {
    'klafyvel/vim-slime-cells',
    requires = {{'jpalardy/vim-slime', ft={'julia'}}},
    ft = {'julia'},
    config=function ()
      vim.g.slime_target = "kitty"
      vim.g.slime_cell_delimiter = "^\\s*##"
      vim.g.slime_dont_ask_default = 1
      vim.g.slime_bracketed_paste = 1
      map('n', '<leader>cv', '<Plug>SlimeConfig')

      -- In NeoVim, for some reason, the SlimeCells commands are not available,
      -- however, the functions are available to be called directly.
      map('n', '<leader>x',  vim.fn['slime_cells#send_cell'])
      map('n', '<C-c><C-c>', vim.fn['slime_cells#send_cell'])
      map('n', 'cj', vim.fn['slime_cells#go_to_next_cell'])
      map('n', 'ck', vim.fn['slime_cells#go_to_previous_cell'])
    end
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
