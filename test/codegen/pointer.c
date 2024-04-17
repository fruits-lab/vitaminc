int main() {
  int a = 10;
  int* b = &a;
  int* c;
  __builtin_print(a);

  *b = 5;
  __builtin_print(a);
  __builtin_print(*b);

  c = b;
  *c = 4;
  __builtin_print(a);
  __builtin_print(*c);

  int* d = c;
  *d = 3;
  __builtin_print(a);
  __builtin_print(*d);

  return 0;
}
