#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
BOLD='\033[1m'
RESET='\033[0m'

passed=0
failed=0

echo ""
echo -e "${BOLD}Running all examples...${RESET}"
echo ""

for file in examples/*.calyx; do
  name=$(basename "$file" .calyx)

  if dune exec ./bin/main.exe -- compile "$file" --backend js >/dev/null 2>&1; then
    echo -e "  ${name} - ${GREEN}success${RESET}"
    ((passed++))
  else
    echo -e "  ${name} - ${RED}failure${RESET}"
    ((failed++))
  fi
done

echo ""
echo -e "${BOLD}Results:${RESET} ${GREEN}${passed} passed${RESET}, ${RED}${failed} failed${RESET}"
echo ""
