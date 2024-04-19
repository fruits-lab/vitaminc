int add(int* x, int* y) {
  return *x + *y;
}

int main() {
  int a = 3;
  int b = 5;
  __builtin_print(add(&a, &b));
  return 0;
}
