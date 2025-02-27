enum {
  FALSE,
  TRUE,
};

int main() {
  // Convertible to int.
  int f = TRUE;
  __builtin_print(f);

  enum Color {
    RED,  // 0
    GREEN = 2,
    BLUE,
  };

  int r = RED;
  enum Color b = GREEN;
  __builtin_print(r);
  __builtin_print(b);

  // Enum constants are visible in the scope even if it's declared with a
  // variable.
  enum {
    MONDAY = 1,
    TUESDAY,
    WEDNESDAY,
    THURSDAY,
    FRIDAY = 5,
    SATURDAY,
    SUNDAY,
  } w = MONDAY;
  int t = TUESDAY;
  __builtin_print(w);
  __builtin_print(t);
  __builtin_print(WEDNESDAY) ;
  __builtin_print(THURSDAY);
  __builtin_print(FRIDAY);
  __builtin_print(SATURDAY);
  __builtin_print(SUNDAY);

  return 0;
}
