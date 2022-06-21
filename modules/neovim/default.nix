{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    vimAlias = true;
    coc = {
      enable = true;
      settings = {
        ocaml = {
          command = "ocamllsp";
          rootPatterns = [ "dune-project" "*.opam" ];
          filetypes = [ "ocaml" "reasonml" ];
        };
      };
    };
    extraConfig = ''
      lua << EOF
      ${builtins.readFile ./init.lua}
      EOF
    '';
    plugins = with pkgs.vimPlugins; with pkgs.vitalityVimPlugins; [
      neon
      nvim-tree-lua
    ];
  };
}
