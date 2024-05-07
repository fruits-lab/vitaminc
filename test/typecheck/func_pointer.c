int add(int a, int b) {
  return a + b;
}

int main() {
  int (*p)(int, int);
  int c = p(2, 3);
  return 0;
}
