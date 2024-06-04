int main() {
  union u;

  union shape {
    int square;
    int circle;
    int triangle;
  };

  union {
    int a;
    int b;
    int c;
    int d;
    int e;
  };

  // It's a legal case, but compilers like GCC may show warning of excessing elements.
  // The value of the members is 3.
  union shape s = {3, 4, 5};
  // Three members in circle variable share the same memory location, so every member is 1.
  union shape circle = {.circle = 1};
  union shape puzzles[3] = {[0].circle = 1, [1].triangle = 2, [2].square = 4};

  return 0;
}
