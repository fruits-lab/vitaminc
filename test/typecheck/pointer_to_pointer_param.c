int add(int** x, int** y) {
  return **x + **y;
}

int main() {
  int a = 3;
  int b = 5;
  int* c = &a;
  int* d = &b;
  return add(&c, &d);
}
