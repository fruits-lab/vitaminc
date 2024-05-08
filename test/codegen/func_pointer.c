int add(int a, int b) {
  return a + b;
}

int main() {
  int (*p)(int, int);
  p = add;
  int c = p(2, 3);
  __builtin_print(c);
  // Taking address of a function has the same effect as using the function
  // name.
  p = &add;
  int d = (*p)(1, 2);
  __builtin_print(d);
  return 0;
}
