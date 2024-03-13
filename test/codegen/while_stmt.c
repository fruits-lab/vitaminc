int main() {
  //
  // while statement
  //
  int i = 0;
  while (i > 0) {
    i = i - 1;
  }
  __builtin_print(i);

  //
  // Nested while statement
  //
  int j = 5;
  while (j > 0) {
    int k = 5;
    while (k > 0) {
      k = k - 1;
    }
    __builtin_print(k);
    j = j - 1;
  }
  __builtin_print(j);

  //
  // do-while statement
  //
  int k = 0;
  do {
    k = k + 1;
  } while (k < 5);
  __builtin_print(k);

  //
  // Nested do-while statement
  //
  int l = 0;
  do {
    int m = 0;
    do {
      m = m + 1;
    } while (m < 5);
    __builtin_print(m);
    l = l + 1;
  } while (l < 5);
  __builtin_print(l);
  return 0;
}
