int main() {
  int i;
  int* p;
  __builtin_print((i = 3, p = &i, i));
  __builtin_print(*p);
  return 0;
}
