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

  bd1.date;

  struct animal {
    int lion;
    int tiger;
    int giraffe;
  } zoo;

  struct book {
    int fiction;
    int sci_fi;
    int history;
  } library = {
    .fiction = 100,
    .sci_fi = 50,
    .history = 500,
  };

  return 0;
}
