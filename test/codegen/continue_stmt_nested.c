int main() {
  int i = 0;
  int x = 0;

  i = 0;
  while (i < 10) {
    int j = 0;
    while (j < 10) {
      j = j + 1;
      if (j > 3) {
        continue;
      }
      x = x + 1;
    }
    i = i + 1;
  }

  i = 0;
  do {
    int j = 0;
    do {
      j = j + 1;
      if (j > 5) {
        continue;
      }
      x = x + 1;
    } while (j < 10);
    i = i + 1;
  } while (i < 10);

  for (i = 0; i < 10; i = i + 1) {
    for (int j = 0; j < 10; j = j + 1) {
      if (j >= 7) {
        continue;
      }
      x = x + 1;
    }
  }

  return x;
}
