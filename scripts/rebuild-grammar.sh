cd tree-sitter-calyx
npx tree-sitter generate
cc -o ~/.local/share/nvim/lazy/nvim-treesitter/parser/calyx.so \
  -shared -fPIC -I./src src/parser.c -Os

# Copy queries to nvim-treesitter and nvim config
mkdir -p ~/.local/share/nvim/lazy/nvim-treesitter/queries/calyx
mkdir -p ~/.config/nvim/queries/calyx
cp queries/*.scm ~/.local/share/nvim/lazy/nvim-treesitter/queries/calyx/
cp queries/*.scm ~/.config/nvim/queries/calyx/
