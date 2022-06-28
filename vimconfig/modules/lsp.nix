{ pkgs, dsl, ... }:
with dsl; {
  plugins = with pkgs; [
    # completion framework
    cmp-nvim-lsp
    nvim-cmp
    cmp-buffer
    # lsp things
    vimPlugins.lsp_signature-nvim
    vimPlugins.lspkind-nvim
    vimPlugins.nvim-lspconfig
    # utility functions for lsp
    vimPlugins.plenary-nvim
    # popout for documentation
    vimPlugins.popup-nvim
  ];
  use.lspconfig.rnix.setup = callWith {
    cmd = [ "${pkgs.rnix-lsp}/bin/rnix-lsp" ];
    capabilities = rawLua
      "require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())";
  };
  lua = builtins.readFile ./lsp.lua;
}
