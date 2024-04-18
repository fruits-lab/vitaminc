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

  int x = 1;
  int y = 2;
  int z = *&x + *&y;
  __builtin_print(z);

  return 0;
}
