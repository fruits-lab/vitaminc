int main() {
  int i;
  int* p;
  // The operands of the comma operator can have any type.
  return (p = &i, i = 0, 3);
}
