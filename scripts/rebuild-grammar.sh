cd tree-sitter-calyx
npx tree-sitter generate
cc -o ~/.local/share/nvim/lazy/nvim-treesitter/parser/calyx.so \
  -shared -fPIC -I./src src/parser.c -Os
