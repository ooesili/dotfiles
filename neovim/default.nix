{pkgs, ...}: let
  inherit (pkgs) vimPlugins;
  mySnippets = {
    pname = "my-nvim-snippets";
    src = ./snippets;
  };

  language-plugins = [
    vimPlugins.elm-vim
    vimPlugins.fennel-vim
    vimPlugins.haskell-vim
    vimPlugins.plantuml-syntax
    vimPlugins.rust-vim
    vimPlugins.typescript-vim
    vimPlugins.vim-glsl
    vimPlugins.vim-helm
    vimPlugins.vim-javascript
    vimPlugins.vim-jsx-pretty
    vimPlugins.vim-nix
    vimPlugins.vim-protobuf
    vimPlugins.vim-pug
    vimPlugins.vim-solidity
    vimPlugins.vim-terraform
    vimPlugins.vim-toml
    vimPlugins.zig-vim
  ];

  general-plugins = [
    vimPlugins.auto-pairs
    vimPlugins.base16-vim
    vimPlugins.blink-cmp
    vimPlugins.conform-nvim
    vimPlugins.diffview-nvim
    vimPlugins.editorconfig-nvim
    vimPlugins.friendly-snippets
    vimPlugins.gitsigns-nvim
    vimPlugins.indent-blankline-nvim
    vimPlugins.kanagawa-nvim
    vimPlugins.lazydev-nvim
    vimPlugins.lualine-nvim
    # vimPlugins.luasnip
    vimPlugins.nvim-dap
    vimPlugins.nvim-dap-go
    vimPlugins.nvim-dap-ui
    vimPlugins.nvim-lint
    vimPlugins.nvim-lspconfig
    vimPlugins.nvim-nio # required by dap-ui
    vimPlugins.nvim-treesitter.withAllGrammars
    vimPlugins.nvim-web-devicons
    vimPlugins.oil-nvim
    vimPlugins.plenary-nvim
    vimPlugins.vim-surround
    vimPlugins.telescope-fzf-native-nvim
    vimPlugins.telescope-nvim
    vimPlugins.telescope-ui-select-nvim
    vimPlugins.tmuxline-vim
    vimPlugins.trouble-nvim
    vimPlugins.vim-abolish
    vimPlugins.vim-better-whitespace
    vimPlugins.vim-commentary
    vimPlugins.vim-endwise
    vimPlugins.vim-eunuch
    vimPlugins.vim-fireplace
    vimPlugins.vim-fugitive
    vimPlugins.vim-illuminate
    vimPlugins.vim-repeat
    vimPlugins.vim-unimpaired
    vimPlugins.which-key-nvim

    mySnippets
  ];
in {
  initLua = "require('myconfig')";

  plugins = {
    start = language-plugins ++ general-plugins;
    dev.myconfig = {
      pure = ./nvim;
      impure = "/home/ooesili/sync/dotfiles/nix-config/neovim/nvim";
    };
  };
}
