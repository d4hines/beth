inputs: final: prev:
let
  withSrc = pkg: src: pkg.overrideAttrs (_: { inherit src; });
  plugin = pname: src: prev.vimUtils.buildVimPluginFrom2Nix {
    inherit pname src;
    version = "master";
  };
in
with inputs; {

  rnix-lsp = inputs.rnix-lsp.packages.${prev.system}.rnix-lsp;

  telescope-nvim = (withSrc prev.vimPlugins.telescope-nvim inputs.telescope-src);
  cmp-buffer = (withSrc prev.vimPlugins.cmp-buffer inputs.cmp-buffer);
  nvim-cmp = (withSrc prev.vimPlugins.nvim-cmp inputs.nvim-cmp);

  cmp-nvim-lsp = withSrc prev.vimPlugins.cmp-nvim-lsp inputs.cmp-nvim-lsp;

  # Example of packaging plugin with Nix
  syntax-tree-surfer = plugin "syntax-tree-surfer" syntax-tree-surfer;
  vim-surround = plugin "vim_surround" vim-surround;
  cmp-nvim-lsp-signature-help = plugin "nvim_lsp_signature_help" cmp-nvim-lsp-signature-help;
  virtual-types = plugin "virtual-types" virtual-types;
  blamer-nvim = plugin "blamer-nvim" blamer-nvim-src;
  comment-nvim = plugin "comment-nvim" comment-nvim-src;
  conceal = plugin "conceal" conceal-src;
  dracula = plugin "dracula" dracula-nvim;
  one_monokai = plugin "one_monokai" one_monokai;
  fidget = plugin "fidget" fidget-src;
  telescope-ui-select = plugin "telescope-ui-select" telescope-ui-select-src;
  which-key = plugin "which-key" which-key-src;
  leap = plugin "leap" leap-src;
  todo-comments = plugin "todo-comments" todo-comments;
  legendary = plugin "legendary" legendary;
}
