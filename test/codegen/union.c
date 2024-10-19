int main() {
  union u;

  union shape {
    int square;
    int circle;
    int triangle;
  };

  // It's a legal case, but compilers like GCC may show warning of excessing elements.
  // The value of the members is 3.
  union shape s = {3, 4, 5};

  // Every member shares the same starting memory address, so their value are the same.
  __builtin_print(s.square);
  __builtin_print(s.circle);
  __builtin_print(s.triangle);

  s.circle = 4;
  __builtin_print(s.square);
  __builtin_print(s.circle);
  __builtin_print(s.triangle);

  union gender {
    int male;
    int female;
  } girl = {
    .female = 1,
  };

  __builtin_print(girl.female);

  return 0;
}
