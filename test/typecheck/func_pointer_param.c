int add(int a, int b) {
  return a + b;
}

int call(int (*p)(int, int), int a, int b) {
  return p(a, b);
}

// Any parameter of function type is adjusted to the corresponding pointer type.
int call_decay(int p(int, int), int a, int b) {
  // do nothing; only testing the type of p
  return 0;
}

int main() {
  int (*c)(int (*)(int, int), int, int) = call;
  c(add, 1, 2);
  c = call_decay;
  c(add, 1, 2);
  return 0;
}
