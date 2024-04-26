int main() {
  int a = 10;
  int* pa = &a;
  int** ppa = &pa;
  __builtin_print(**ppa);

  int b = 20;
  int* pb = &b;
  *ppa = pb;
  __builtin_print(**ppa);

  **ppa = 30;
  __builtin_print(b);

  return 0;
}
