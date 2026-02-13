#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
BOLD='\033[1m'
RESET='\033[0m'

passed=0
failed=0

echo ""
echo -e "${BOLD}Running all examples...${RESET}"

# Should-succeed: these must compile successfully
echo ""
echo -e "${BOLD}Should succeed:${RESET}"
for file in examples/should-succeed/*.calyx; do
  name=$(basename "$file" .calyx)

  if dune exec ./bin/main.exe -- build "$file" >/dev/null 2>&1; then
    echo -e "  ${name} - ${GREEN}pass${RESET}"
    ((passed++))
  else
    echo -e "  ${name} - ${RED}FAIL (expected success)${RESET}"
    ((failed++))
  fi
done

# Should-fail: these must fail to compile
echo ""
echo -e "${BOLD}Should fail:${RESET}"
for file in examples/should-fail/*.calyx; do
  name=$(basename "$file" .calyx)

  if dune exec ./bin/main.exe -- build "$file" >/dev/null 2>&1; then
    echo -e "  ${name} - ${RED}FAIL (expected failure)${RESET}"
    ((failed++))
  else
    echo -e "  ${name} - ${GREEN}pass${RESET}"
    ((passed++))
  fi
done

echo ""
echo -e "${BOLD}Results:${RESET} ${GREEN}${passed} passed${RESET}, ${RED}${failed} failed${RESET}"

if [ $failed -gt 0 ]; then
  exit 1
fi
