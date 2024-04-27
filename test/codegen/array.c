int main() {
  int a[4];

  a[0] = 1;
  __builtin_print(a[0]);

  a[1] = 2;
  a[2] = a[1];
  __builtin_print(a[2]);

  a[3] = 2 + a[2];
  __builtin_print(a[3]);

  return 0;
}
