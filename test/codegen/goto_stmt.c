int main() {
  goto first;
second:
  __builtin_print(2);
  goto third;
first:
  __builtin_print(1);
  goto second;
third:
  __builtin_print(3);

  return 0;
}
