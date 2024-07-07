int main() {
  int a[4];

  a[0] = 1;
  __builtin_print(a[0]);

  a[1] = 2;
  a[2] = a[1];
  __builtin_print(a[2]);

  a[3] = 2 + a[2];
  __builtin_print(a[3]);

  int b = 2, c = 0, d[4] = {1, b, c = 3, 4};
  __builtin_print(d[0]);
  __builtin_print(d[1]);
  __builtin_print(d[2]);
  __builtin_print(d[3]);

  int e[3] = {1};
  __builtin_print(e[0]);
  __builtin_print(e[1]);
  __builtin_print(e[2]);

  // NOTE: Local scope array should not be 0-initialized, and the generated IR has to be checked manually.
  int f[2];
  return 0;
}
