int main() {
  int a = 2;
  int b = 1;
  switch (a) {
    case 1:
      switch (b) {
        case 1:
          b = 2 + a;
          break;
        case 2:
          b = 3 + a;
          break;
        default:
          break;
      }
      break;
    case 2:
      switch (b) {
        case 1:
          b = 3 + a;  // <- Should execute this line.
          break;
        case 2:
          b = 4 + a;
          break;
        default:
          break;
      }
      break;
    default:
      switch (b) {
        case 1:
          b = 4 + a;
          break;
        case 2:
          b = 5 + a;
          break;
        default:
          break;
      }
  }
  return b;
}
