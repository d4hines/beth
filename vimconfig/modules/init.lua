vim.opt.timeoutlen = 100
vim.opt.signcolumn = "yes"

vim.g.mapleader = " "
vim.g.noswapfile = true
vim.g.blamer_enabled = 1

-- TODO: remove or at least inspect this stuff
vim.o.undofile = true
vim.o.showcmd = true;
vim.o.showmatch = true;
vim.o.ignorecase = true;
vim.o.smartcase = true;
vim.o.cursorline = true;
vim.o.wrap = true;
vim.o.autoindent = true;
vim.o.copyindent = true;
vim.o.splitbelow = true;
vim.o.splitright = true;
vim.o.number = true;
vim.o.relativenumber = true;
vim.o.title = true;
vim.o.undofile = true;
vim.o.autoread = true;
vim.o.hidden = true;
vim.o.list = true;
vim.o.background = "dark";
vim.o.backspace = "indent,eol,start";
vim.o.undolevels = 1000000;
vim.o.undoreload = 1000000;
vim.o.foldmethod = "indent";
vim.o.foldnestmax = 10;
vim.o.foldlevel = 1;
vim.o.scrolloff = 3;
vim.o.sidescrolloff = 5;
vim.o.listchars = "tab:→→,trail:●,nbsp:○";
vim.o.clipboard = "unnamed,unnamedplus";
vim.o.formatoptions = "tcqj";
vim.o.encoding = "utf-8";
vim.o.fileencoding = "utf-8";
vim.o.fileencodings = "utf-8";
vim.o.bomb = true;
vim.o.binary = true;
vim.o.matchpairs = "(:),{:},[:],<:>";
vim.o.expandtab = true;
vim.o.wildmode = "list:longest,list:full";

-------------- Git --------------
require('gitsigns').setup { }
require('neogit').setup { }

---------- Aesthetics -----------
require('one_monokai').setup { }

require('tabline').setup { }
require('lualine').setup { }

require('todo-comments').setup { }

---------- Treesitter ----------
require('Comment').setup()

require'nvim-treesitter.configs'.setup {
  ensure_installed = {
    "nix",
    "rust",
    "ocaml",
    "javascript",
    "typescript"
  },
  highlight = {
    enable = true,
  },
}