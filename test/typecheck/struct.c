int main() {
  struct ss;

  struct birth {
    int date;
    int month;
    int year;
  };

  struct {
    int quarter;
    int dime;
    int penny;
  };

  struct birth bd1 = {
    .date = 1,
    .month = 1,
    .year = 1995,
  };

  struct birth bd2 = {3, 3, 1998};

  struct birth bd3[3] = {[0].date = 4, [1].year = 1999};

  struct birth a, *b, c[3];

  return 0;
}
