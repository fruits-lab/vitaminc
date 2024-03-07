int sum(int a, int b, int c) {
  return a + b + c;
}

int add_five(int d) {
  return d + 5;
}

int main() {
  int a = sum(1, 2, 3);
  int b = a + 4;
  return add_five(b);
}
