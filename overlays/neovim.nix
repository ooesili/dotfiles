_final: prev: let
  inherit (prev.vimUtils.override {vim = prev.neovim;}) buildVimPlugin;

  initLua = prev.substituteAll {
    src = ../pkgs/neovim-config/nvim/init.lua;
    goTemplateFile = ../pkgs/neovim-config/go-templates/main.go;
  };

  myConfig = buildVimPlugin {
    name = "my-nvim-config";
    src = ../pkgs/neovim-config/nvim;
  };

  mySnippets = buildVimPlugin {
    name = "my-nvim-snippets";
    src = ../pkgs/neovim-config/snippets;
  };

  vimPlugins =
    prev.vimPlugins
    // {
      nvim-treesitter = prev.vimPlugins.nvim-treesitter.withAllGrammars;
    };

  language-plugins = [
    vimPlugins.elm-vim
    vimPlugins.fennel-vim
    vimPlugins.haskell-vim
    vimPlugins.plantuml-syntax
    vimPlugins.rust-vim
    vimPlugins.typescript-vim
    vimPlugins.vim-glsl
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
    vimPlugins.ale
    vimPlugins.auto-pairs
    vimPlugins.base16-vim
    vimPlugins.cmp-buffer
    vimPlugins.cmp-cmdline
    vimPlugins.cmp-nvim-lsp
    vimPlugins.cmp-nvim-lsp-signature-help
    vimPlugins.cmp-path
    vimPlugins.cmp_luasnip
    vimPlugins.diffview-nvim
    vimPlugins.editorconfig-nvim
    vimPlugins.friendly-snippets
    vimPlugins.gitsigns-nvim
    vimPlugins.indent-blankline-nvim
    vimPlugins.lualine-nvim
    vimPlugins.luasnip
    vimPlugins.neodev-nvim
    vimPlugins.null-ls-nvim
    vimPlugins.nvim-cmp
    vimPlugins.nvim-lspconfig
    vimPlugins.nvim-treesitter
    vimPlugins.nvim-treesitter-textobjects
    vimPlugins.nvim-web-devicons
    vimPlugins.oil-nvim
    vimPlugins.plenary-nvim
    vimPlugins.surround
    vimPlugins.telescope-fzf-native-nvim
    vimPlugins.telescope-nvim
    vimPlugins.telescope-ui-select-nvim
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
  ];
in {
  neovim = prev.neovim.override {
    configure = {
      customRC = "luafile ${initLua}";

      packages.myVimPackage.start =
        [myConfig mySnippets]
        ++ language-plugins
        ++ general-plugins;
    };
  };
}
