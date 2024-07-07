int c;
int d = 6;

int b[2];
int a[3] = {6, 5, 3};
int e[4] = {8, 9};

int main() {
  __builtin_print(c);
  d = 4;
  __builtin_print(d);

  __builtin_print(b[0]);

  a[0] = 7;
  a[2] = 4;
  __builtin_print(a[0]);
  __builtin_print(a[1]);
  __builtin_print(a[2]);

  __builtin_print(e[0]);
  __builtin_print(e[1]);
  __builtin_print(e[2]);
  __builtin_print(e[3]);
  return 0;
}
