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
