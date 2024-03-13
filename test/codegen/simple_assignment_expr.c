int main() {
  int i;
  int j;
  __builtin_print(i = 0);
  i = (j = 1 + 2 + 3 + 4) + 5;
  __builtin_print(i);
  __builtin_print(j);
  return 0;
}
