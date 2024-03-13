int main() {
  //
  // if statement
  //
  if (1) {
    __builtin_print(1);
  }
  __builtin_print(2);

  //
  // if-else statement
  //
  if (1) {
    __builtin_print(3);
  } else {
    __builtin_print(4);
  }

  //
  // Nested if-else statement
  // Each part is consist of another if-else statement and an if statement.
  //
  if (1) {
    if (1) {
      __builtin_print(5);
    } else {
      __builtin_print(6);
    }
    if (1) {
      __builtin_print(7);
    }
    __builtin_print(8);
  } else {
    __builtin_print(9);
    if (1) {
      __builtin_print(10);
    } else {
      __builtin_print(11);
    }
    if (1) {
      __builtin_print(12);
    }
    __builtin_print(13);
  }
  __builtin_print(14);

  return 0;
}
