int main() {
  int i = 25;
  if (i < 10) {
    if (i < 5) {
      i = 2;
    } else {
      i = 7;
    }
  } else {
    if (i > 20) {
      i = 30;
    } else {
      i = 15;
    }
  }

  return i;
}
