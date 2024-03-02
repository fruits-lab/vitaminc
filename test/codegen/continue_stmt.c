int main() {
  int i = 0;
  int x = 0;

  while (i < 10) {
    i = i + 1;
    if (i > 3) {
      continue;
    }
    x = x + 1;
  }

  i = 0;
  do {
    i = i + 1;
    if (i > 5) {
      continue;
    }
    x = x + 1;
  } while (i < 10);

  for (i = 0; i < 10; i = i + 1) {
    if (i >= 7) {
      continue;
    }
    x = x + 1;
  }

  return x;
}
