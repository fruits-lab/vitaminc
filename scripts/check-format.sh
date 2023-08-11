#!/usr/bin/env sh

#
# Exits silently upon success;
# otherwise, exits with the count of violations.
#

SOURCES=$(find "$(git rev-parse --show-toplevel)" | grep -E "\.(c|h)pp\$")

# reports violations
clang-format --dry-run "${SOURCES}"

VIOLATION_COUNT="$(clang-format --output-replacements-xml "${SOURCES}" | grep -E -c "</replacement>")"
exit "$VIOLATION_COUNT"
