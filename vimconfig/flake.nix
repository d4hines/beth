{
  description = "Tutorial Flake accompanying vimconf talk.";

  # Input source for our derivation
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    terraform-ls-src = {
      url = "github:hashicorp/terraform-ls";
      flake = false;
    };
    nix2vim = {
      url = "github:gytis-ivaskevicius/nix2vim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim = { url = "github:neovim/neovim?dir=contrib&ref=333ba6569d833e22c0d291547d740d4bbfa3fdab"; };
    telescope-src = {
      url = "github:nvim-telescope/telescope.nvim";
      flake = false;
    };
    one_monokai = {
      url = "github:cpea2506/one_monokai.nvim";
      flake = false;
    };
    dracula-nvim = {
      url = "github:Mofiqul/dracula.nvim";
      flake = false;
    };
    nvim-cmp = {
      url = "github:hrsh7th/nvim-cmp";
      flake = false;
    };
    cmp-nvim-lsp = {
      url = "github:hrsh7th/cmp-nvim-lsp";
      flake = false;
    };
    cmp-buffer = {
      url = "github:hrsh7th/cmp-buffer";
      flake = false;
    };
    cmp-nvim-lsp-signature-help = {
      url = "github:hrsh7th/cmp-nvim-lsp-signature-help";
      flake = false;
    };
    rnix-lsp = {
      url = "github:nix-community/rnix-lsp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    comment-nvim-src = {
      url = "github:numToStr/Comment.nvim";
      flake = false;
    };
    blamer-nvim-src = {
      url = "github:APZelos/blamer.nvim";
      flake = false;
    };
    telescope-ui-select-src = {
      url = "github:nvim-telescope/telescope-ui-select.nvim";
      flake = false;
    };
    virtual-types = {
      url = "github:jubnzv/virtual-types.nvim";
      flake = false;
    };
    fidget-src = {
      url = "github:j-hui/fidget.nvim";
      flake = false;
    };
    neogen-src = {
      url = "github:danymat/neogen";
      flake = false;
    };
    which-key-src = {
      url = "github:folke/which-key.nvim?ref=bd4411a2ed4dd8bb69c125e339d837028a6eea71";
      flake = false;
    };
    conceal-src = {
      url = "github:ticki/rust-cute-vim";
      flake = false;
    };
    colorizer-src = {
      url = "github:powerman/vim-plugin-AnsiEsc";
      flake = false;
    };
    guess-indent-src = {
      url = "github:NMAC427/guess-indent.nvim";
      flake = false;
    };
    leap-src = {
      url = "github:ggandor/leap.nvim";
      flake = false;
    };
    todo-comments = {
      url = "github:folke/todo-comments.nvim";
      flake = false;
    };
    legendary = {
      url = "github:mrjones2014/legendary.nvim";
      flake = false;
    };
  };

  outputs = inputs@{ self, flake-utils, nixpkgs, neovim, nix2vim, ... }:
    let
      myOverlay = final: prev:
        let overlays = [
          (neovim.overlay)
          neovim.overlay
          (import ./plugins.nix inputs)
          nix2vim.overlay
          (final: prev:
            {
              neovim = prev.neovimBuilder {
                # Build with NodeJS
                withNodeJs = true;
                withPython3 = true;
                package = prev.neovim;
                imports = [
                  ./modules/init.nix
                  ./modules/lsp.nix
                  ./modules/telescope.nix
                  # ./modules/treesitter.nix
                  ./modules/wilder.nix
                ];
              };
            })
        ];
        in
        with prev;
        lib.foldl' (lib.flip lib.extends) (lib.const prev) overlays final;
    in
    { overlays = { default = myOverlay; }; }
    //
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs {
        inherit system;
        overlays = [ myOverlay ];
      };
      in
      {
        # The package built by `nix build .`
        defaultPackage = pkgs.neovim;
        # The app run by `nix run .`
        apps.defaultApp = {
          type = "app";
          program = "${pkgs.neovim}/bin/nvim";
          
        };
        devShell = pkgs.mkShell {
                packages = with pkgs; with ocamlPackages; [
                        cowsay
                        dune_2
                        ocaml
                        ocaml-lsp
                ];
          };
      }
    );
}

