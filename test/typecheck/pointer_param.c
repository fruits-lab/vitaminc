int add(int* x, int* y) {
  return *x + *y;
}

int main() {
  int a = 3;
  int b = 5;
  int* c = &b;
  return add(&a, c);
}
