int main() {
  __builtin_print(7 % 6 + 5 * 4 - 3 / 2 + 1);
  __builtin_print(5 | 4);
  __builtin_print(5 ^ 4);
  __builtin_print(5 & 4);
  __builtin_print(1 << 16);
  __builtin_print(-1 << 31);
  __builtin_print(65536 >> 16);
  __builtin_print(-1 >> 16);
  __builtin_print(4 && 3);
  __builtin_print(0 && 2);
  __builtin_print(5 || 0);
  __builtin_print(0 || 0);
  __builtin_print((1, 2));
  return 0;
}
