cd tree-sitter-calyx
npx tree-sitter generate

# nvim-treesitter (main branch) loads parsers/queries from the runtimepath
# 'site' directory, not from inside the plugin checkout.
PARSER_DIR=~/.local/share/nvim/site/parser
QUERY_DIR=~/.local/share/nvim/site/queries/calyx

mkdir -p "$PARSER_DIR" "$QUERY_DIR"

cc -o "$PARSER_DIR/calyx.so" \
  -shared -fPIC -I./src src/parser.c -Os

# Copy queries to the runtimepath and (legacy) nvim config locations
cp queries/*.scm "$QUERY_DIR/"
mkdir -p ~/.config/nvim/queries/calyx
cp queries/*.scm ~/.config/nvim/queries/calyx/
