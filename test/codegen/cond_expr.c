int main() {
  int i = 0;
  int j = 5;
  i = (1 == 2 ? i + 1 : i - 1);
  __builtin_print(i);
  j = (3 == 3 ? j + 1 : j - 1);
  __builtin_print(j);
  return 0;
}
