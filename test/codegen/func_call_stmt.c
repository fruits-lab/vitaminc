/* Function definitions are tested together since one cannot test function calls
 * without function definitions. */

int five(/* no parameter */) {
  return 5;
}

int sum(int a, int b, int c) {
  return a + b + c;
}

int add_five(int d) {
  return d + five();
}

int main() {
  __builtin_print(add_five(sum(1, 2, 3) + 4));
  return 0;
}
