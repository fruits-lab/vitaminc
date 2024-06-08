int main() {
  int j = 0;
  int i;
  for (i = 0 /* expr */; i < 5; i = i + 1) {
    j = j + 1;
  }
  for (int a = 0, b = 5 /* decl */; a < b; a = a + 1) {
    j = j + 1;
  }

  return 0;
}
