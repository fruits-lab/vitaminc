int main() {
  int a[3];

  a[0] = 1;
  a[1] = 2;
  a[2] = a[1] + 1;

  int b = 0;
  int c[4] = {1, b = 2, 3, 4};

  return 0;
}
