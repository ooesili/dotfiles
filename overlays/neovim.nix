final: prev:

let
  inherit (prev.vimUtils.override { vim = prev.neovim; }) buildVimPluginFrom2Nix;

  initLua = prev.substituteAll {
    src = ../pkgs/neovim-config/nvim/init.lua;
    goTemplateFile = ../pkgs/neovim-config/go-templates/main.go;
  };

  myConfig = buildVimPluginFrom2Nix {
    pname = "my-nvim-config";
    src = ../pkgs/neovim-config/nvim;
    version = "latest";
  };

  vimPlugins = prev.vimPlugins // {
    nvim-treesitter = prev.vimPlugins.nvim-treesitter.withPlugins (plugins: [
      plugins.tree-sitter-bash
      plugins.tree-sitter-c
      plugins.tree-sitter-clojure
      plugins.tree-sitter-dockerfile
      plugins.tree-sitter-fennel
      plugins.tree-sitter-go
      plugins.tree-sitter-html
      plugins.tree-sitter-javascript
      plugins.tree-sitter-json
      plugins.tree-sitter-lua
      plugins.tree-sitter-make
      plugins.tree-sitter-markdown
      plugins.tree-sitter-nix
      plugins.tree-sitter-python
      plugins.tree-sitter-ruby
      plugins.tree-sitter-rust
      plugins.tree-sitter-scss
      plugins.tree-sitter-toml
      plugins.tree-sitter-typescript
      plugins.tree-sitter-vue
      plugins.tree-sitter-yaml
      plugins.tree-sitter-zig
    ]);
  };

in {
  neovim = prev.neovim.override {
    configure = {
      customRC = "luafile ${initLua}";

      packages.myVimPackage.start = [
        myConfig

        vimPlugins.ale
        vimPlugins.aniseed
        vimPlugins.auto-pairs
        vimPlugins.base16-vim
        vimPlugins.conjure
        vimPlugins.dhall-vim
        vimPlugins.elm-vim
        vimPlugins.fennel-vim
        vimPlugins.haskell-vim
        vimPlugins.nvim-lspconfig
        vimPlugins.nvim-treesitter
        vimPlugins.nvim-treesitter-context
        vimPlugins.plantuml-syntax
        vimPlugins.plenary-nvim
        vimPlugins.rust-vim
        vimPlugins.surround
        vimPlugins.telescope-fzf-native-nvim
        vimPlugins.telescope-nvim
        vimPlugins.telescope-ui-select-nvim
        vimPlugins.typescript-vim
        vimPlugins.vim-abolish
        vimPlugins.vim-airline
        vimPlugins.vim-airline-themes
        vimPlugins.vim-better-whitespace
        vimPlugins.vim-commentary
        vimPlugins.vim-endwise
        vimPlugins.vim-eunuch
        vimPlugins.vim-fugitive
        vimPlugins.vim-glsl
        vimPlugins.vim-javascript
        vimPlugins.vim-jsx-pretty
        vimPlugins.vim-nix
        vimPlugins.vim-protobuf
        vimPlugins.vim-pug
        vimPlugins.vim-repeat
        vimPlugins.vim-solidity
        vimPlugins.vim-terraform
        vimPlugins.vim-toml
        vimPlugins.vim-unimpaired
        vimPlugins.zig-vim
      ];
    };
  };
}
