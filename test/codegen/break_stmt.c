int main() {
  int i = 0;
  int x = 0;

  //
  // break statement in un-nested loops
  //
  i = 0;
  x = 0;
  while (i < 10) {
    if (i == 3) {
      break;
    }
    x = x + 1;
    i = i + 1;
  }
  __builtin_print(x);

  i = 0;
  x = 0;
  do {
    if (i == 5) {
      break;
    }
    x = x + 1;
    i = i + 1;
  } while (i < 10);
  __builtin_print(x);

  i = 0;
  x = 0;
  for (; i < 10; i = i + 1) {
    if (i == 7) {
      break;
    }
    x = x + 1;
  }
  __builtin_print(x);

  //
  // break statement in nested loops
  //
  i = 0;
  x = 0;
  while (i < 10) {
    int j = 0;
    while (j < 10) {
      if (j == 3) {
        break;
      }
      x = x + 1;
      j = j + 1;
    }
    i = i + 1;
  }
  __builtin_print(x);

  i = 0;
  x = 0;
  do {
    int j = 0;
    do {
      if (j == 5) {
        break;
      }
      x = x + 1;
      j = j + 1;
    } while (j < 10);
    i = i + 1;
  } while (i < 10);
  __builtin_print(x);

  i = 0;
  x = 0;
  for (; i < 10; i = i + 1) {
    for (int j = 0; j < 10; j = j + 1) {
      if (j == 7) {
        break;
      }
      x = x + 1;
    }
  }
  __builtin_print(x);

  return 0;
}
