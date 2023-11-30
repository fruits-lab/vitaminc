int main() {
  int i = 5;
  if (i < 10)
    if (i < 5)
      i = 2;
    else
      i = 7;
  else
    i = 15;

  return i;
}
