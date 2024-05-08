int add(int a, int b) {
  return a + b;
}

int main() {
  int (*p)(int, int);
  p = add;
  int c = p(2, 3);
  p = &add;
  int d = p(1, 2);
  return 0;
}
