int main() {
  struct ss;

  struct birth {
    int date;
    int month;
    int year;
  };

  // Compiler will stop initializing elements at index 4 since we only have
  // 3 members defined in `struct birth`.
  struct birth bd1 = {3, 5, 1998, 2000, 2002};

  __builtin_print(bd1.date);
  __builtin_print(bd1.month);
  __builtin_print(bd1.year);

  bd1.date = bd1.date + 2;
  __builtin_print(bd1.date);

  return 0;
}
