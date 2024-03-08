int main() {
  int a = 3;
  switch (a) {
    case 1:
      a = a + 2;
      break;
    default:
      a = a + 4;  // <- Should execute this line.
    case 2:
      a = a + 3;  // <- And then fall through to this line.
  }
  return a;
}
