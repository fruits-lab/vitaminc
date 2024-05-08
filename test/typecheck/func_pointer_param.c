int add(int a, int b) {
  return a + b;
}

int call(int (*p)(int, int), int a, int b) {
  return p(a, b);
}

int main() {
  int (*c)(int (*)(int, int), int, int) = call;
  c(add, 1, 2);
  return 0;
}
