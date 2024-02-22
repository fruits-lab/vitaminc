#!/usr/bin/env sh

#
# Runs clang-tidy on the diff between main and HEAD.
# Note: This script is intended to be run in CI.
#

set -eu

if [ -z "${GITHUB_ACTIONS+x}" ]; then
  echo "error: this script is intended to be run in CI"
  exit 1
fi

CLANG_TIDY_DIFF=${CLANG_TIDY_DIFF:-clang-tidy-diff.py}

command -v bear >/dev/null 2>&1 || (
  echo "error: bear is required to generate compile_commands.json; see https://github.com/rizsotto/Bear"
  exit 1
)

# Run a clean build to generate compile_commands.json.
make clean
bear -- make
git fetch origin "${GITHUB_BASE_REF}:refs/remotes/origin/${GITHUB_BASE_REF}"
git diff -U0 "origin/${GITHUB_BASE_REF}" | ${CLANG_TIDY_DIFF} -p1
