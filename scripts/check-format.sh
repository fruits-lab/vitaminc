#!/usr/bin/env sh

#
# Exits silently upon success;
# otherwise, exits with the count of violations.
#

SOURCES=$(find "$(git rev-parse --show-toplevel)" | grep -E "\.(c|h)pp\$")

total_violation_count=0
for file in $SOURCES; do
  # reports violations
  clang-format --dry-run "$file"
  # count violations
  violation_count=$(clang-format --output-replacements-xml "$file" | grep -E -c "</replacement>")
  total_violation_count=$((total_violation_count + violation_count))
done
exit "$total_violation_count"
