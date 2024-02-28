int main() {
  int i = 0;
  int x = 0;

  while (i < 10) {
    if (i == 3) {
      break;
    }
    x = x + 1;
    i = i + 1;
  }

  i = 0;
  do {
    if (i == 5) {
      break;
    }
    x = x + 1;
    i = i + 1;
  } while (i < 10);

  for (i = 0; i < 10; i = i + 1) {
    if (i == 7) {
      break;
    }
    x = x + 1;
  }

  return x;
}
