/* Note that we cannot test whether a uninitialized auto variable has garbage
 * value or not since such value in non-deterministic.
 * Regardless of the issue mention above, the code generation should success
 * gracefully. */

int main() {
  int i;
  int j = 2;
  return j;
}
