enum {
  FALSE,
  TRUE,
};

int main() {
  int f = TRUE;

  enum Color {
    RED,  // 0
    GREEN = 2,
    BLUE,
  };

  // Convertible to int.
  int r = RED;
  enum Color b = GREEN;
  int c = b;

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

  return 0;
}
