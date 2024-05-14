// Any parameter of array type is adjusted to the corresponding pointer type.
int func(int a[3]) {
  // do nothing; only testing the type of a
  return 0;
}

int main() {
  int a[3] = {1, 2, 3};
  func(a);
  return 0;
}
