int main() {
  union shape {
    int square;
    int circle;
    int triangle;
  };

  // TODO: For the following example, each member is set to 6, with compiler warning for Clang.
  // union shape s = {.circle = 5, .triangle = 6};
  union shape s = {.circle = 5};

  // Every member shares the same starting memory address, so their value are the same.
  __builtin_print(s.square);
  __builtin_print(s.circle);
  __builtin_print(s.triangle);

  return 0;
}
