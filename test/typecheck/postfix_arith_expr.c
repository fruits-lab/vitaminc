int main() {
  int a = 0;
  a++;
  a--;

  int* b = &a;
  b++;
  // *b is now garbage value
  b--;
  // *b is now a

  return a;
}
