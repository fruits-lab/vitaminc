int main() {
  int a = 10;
  int* b;
  int* c = &a;
  b = c;
  *c = 5;

  return *c;
}
