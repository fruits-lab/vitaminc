int main() {
  struct birth {
    int date;
    int month;
    int year;
  };

  struct birth bd1 = {
    .year = 1995,
    .date = 3,
    .month = 4,
  };

  __builtin_print(bd1.date);
  __builtin_print(bd1.month);
  __builtin_print(bd1.year);

  return 0;
}
