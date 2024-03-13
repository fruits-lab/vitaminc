int main() {
  //
  // for loop
  //
  int j = 0;
  for (int i = 0; i < 5; i = i + 1) {
    j = j + 1;
  }
  __builtin_print(j);

  //
  // Nested for loop
  //
  int k = 0;
  for (int i = 0; i < 5; i = i + 1) {
    for (int j = 0; j < 5; j = j + 1) {
      k = k + 1;
    }
  }
  __builtin_print(k);
  return 0;
}
