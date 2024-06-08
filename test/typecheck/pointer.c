int main() {
  int a = 10;
  int* b;
  int* c = &a;
  b = c;
  *c = 5;

  int *x, y, *z = &c;

  return *c;
}
