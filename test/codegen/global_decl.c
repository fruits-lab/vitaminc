int c;
int d = 6;

int b[2];
int a[3] = {6, 5, 3};

int main() {
  __builtin_print(c);
  __builtin_print(d);

  __builtin_print(b[0]);
  __builtin_print(a[0]);
  __builtin_print(a[1]);
  __builtin_print(a[2]);
  return 0;
}
