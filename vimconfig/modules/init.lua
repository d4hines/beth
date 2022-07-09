vim.opt.timeoutlen = 1000
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

local current_signature = function(width)
  local sig = require("lsp_signature").status_line(width)
  return sig.label .. "🐼" .. sig.hint
end

require('lualine').setup {
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {'filename'},
    lualine_x = { current_signature },
    lualine_y = {'encoding', 'fileformat', 'filetype'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
}

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

require('legendary').setup()
local wk = require("which-key")
-- mappings ripped from https://docs.helix-editor.com/keymap.html
wk.register({
  g = {
        d = { vim.lsp.buf.definition, "Go to definition"},
        y = { vim.lsp.buf.type_definition, "Go to type definition"},
        r = { vim.lsp.buf.references, "Go to references"},
        i = { vim.lsp.buf.implementation, "Go to implementation"},
        a = {":b#<cr>", "Go to alternate file"},
        n = {":bn<cr>", "Go to next buffer"},
        p = {":bp<cr>", "Go to previous buffer"},
        ["."] = {"`.", "Go to last modification"},
  },
  K = { vim.lsp.buf.hover, "Show LSP documentation"},
  ["<leader>"] = {
    name = "file", -- optional group name
    f = { "<cmd>Telescope find_files<cr>", "Open file picker" }, -- create a binding with label
    b = { "<cmd>Telescope buffers<cr>", "Open buffer picker" }, -- additional options for creating the keymap
    s = { "<cmd>Telescope lsp_document_symbols<cr>", "Open document symbol picker"},
    S = { "<cmd>Telescope lsp_workspace_sybmols<cr>", "Open workspace symbol picker"},
    r = { vim.lsp.buf.rename, "Rename symbol"},
    a = { vim.lsp.buf.code_action, "Apply code action"},
    ["/"] = { "<cmd>Telescope live_grep<cr>", "Global search workspace"},
    w = {
        s = {"<cmd>sp<cr>", "Split window horizontally"},
        v = {"<cmd>vs<cr>", "Split window horizontally"},
        h = {"<cmd>wincmd h<cr>", "Move to left split"},
        j = {"<cmd>wincmd j<cr>", "Move to split below"},
        k = {"<cmd>wincmd k<cr>", "Move to split above"},
        l = {"<cmd>wincmd l<cr>", "Move to right split"},
        d = {"<cmd>q<cr>", "Close current window"},
    },
    ["<leader>"] = {"<c-w><c-w>", "Go to next window"},
    h = {
    },
  },
})

