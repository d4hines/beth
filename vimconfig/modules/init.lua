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

local wk = require("which-key")
-- As an example, we will create the following mappings:
--  * <leader>ff find files
--  * <leader>fr show recent files
--  * <leader>fb Foobar
-- we'll document:
--  * <leader>fn new file
--  * <leader>fe edit file
-- and hide <leader>1

wk.register({
  g = {
        d = { function() print("FIXME:") end, "Go to definition"},
        y = { function() print("FIXME:") end, "Go to type definition"},
        r = { function() print("FIXME:") end, "Go to references"},
        i = { function() print("FIXME:") end, "Go to implementation"},
        a = {":b#<cr>", "Go to alternate file"},
        n = {":bn<cr>", "Go to next buffer"},
        p = {":bp<cr>", "Go to previous buffer"},
        ["."] = {"`.", "Go to last modification"},
  },
  ["<leader>"] = {
    name = "file", -- optional group name
    f = { "<cmd>Telescope find_files<cr>", "Open file picker" }, -- create a binding with label
    b = { "<cmd>Telescope buffers<cr>", "Open buffer picker" }, -- additional options for creating the keymap
    s = { function () print("FIXME:") end, "Open document symbol picker"},
    S = { function () print("FIXME:") end, "Open workspace symbol picker"},
    S = { function () print("FIXME:") end, "Open workspace symbol picker"},
    n = { "New File" }, -- just a label. don't create any mapping
    e = "Edit File", -- same as above
    ["1"] = "which_key_ignore",  -- special label to hide it in the popup
    b = { function() print("bar") end, "Foobar" } -- you can also pass functions!
  },
})

