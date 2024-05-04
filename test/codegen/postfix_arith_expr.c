int main() {
  int a = 0;

  __builtin_print(a++);
  __builtin_print(a--);

  int b = a++;
  __builtin_print(b++);

  return 0;
}
