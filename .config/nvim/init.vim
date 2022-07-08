call plug#begin()
Plug 'tpope/vim-commentary'
Plug 'ap/vim-css-color'
Plug 'tpope/vim-surround'
Plug 'Pocco81/AutoSave.nvim'
Plug 'hoob3rt/lualine.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'arcticicestudio/nord-vim'
call plug#end()

set clipboard=unnamedplus
set noswapfile
set nobackup 
set number relativenumber
set title
set nobackup
set nohlsearch
set showcmd 
set cmdheight=1 
set laststatus=2
set scrolloff=10
set mouse=a
set nosc noru nosm
set lazyredraw
set ignorecase
set nowrap
set cursorline
set tabstop=8 
set softtabstop=0 
set expandtab 
set shiftwidth=4 
set smarttab
set termguicolors

colorscheme nord

lua << END
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'nord',
    disabled_filetypes = {},
    always_divide_middle = true,
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {{
      'filename',
      file_status = true,
      path = 0
    }},
    lualine_x = {'diagnostics', 'encoding', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {{
      'filename',
      file_status = true,
      path = 1
    }},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {}
}
END

lua << EOF
local autosave = require("autosave")
autosave.setup(
    {
        enabled = true,
        execution_message = '',
        events = {"InsertLeave", "TextChanged"},
        conditions = {
            exists = true,
            filename_is_not = {},
            filetype_is_not = {},
            modifiable = true
        },
        write_all_buffers = false,
        on_off_commands = true,
        clean_command_line_interval = 0,
        debounce_delay = 135
    }
)
EOF

