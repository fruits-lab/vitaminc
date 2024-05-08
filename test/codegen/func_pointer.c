int add(int a, int b) {
  return a + b;
}

int main() {
  int (*p)(int, int);
  p = add;
  int c = p(2, 3);
  __builtin_print(c);
  return 0;
}
