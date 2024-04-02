int main() {
  int i = 0;
  {
  begin:
    i = i + 1;
  }
  // Since label is of function scope,
  // it should still be visible here.
  goto begin;
  return 0;
}
