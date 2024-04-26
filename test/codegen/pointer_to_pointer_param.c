int add(int** x, int** y) {
  return **x + **y;
}

int main() {
  int a = 3;
  int b = 5;
  int* c = &a;
  int* d = &b;
  __builtin_print(add(&c, &d));
  return 0;
}
