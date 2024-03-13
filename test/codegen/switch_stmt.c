int main() {
  //
  // switch statement
  //
  switch (3) {
    case 0:
      __builtin_print(1);
      break;
    case 3:
      __builtin_print(2);
      break;
    default:
      __builtin_print(3);
  }
  __builtin_print(4);

  //
  // switch statement default in the middle and fall through
  //
  switch (3) {
    case 1:
      __builtin_print(5);
      break;
    default:
      __builtin_print(6);  // <- Should execute this line.
    case 2:
      __builtin_print(7);  // <- And then fall through to this line.
  }
  __builtin_print(8);

  //
  // Nested switch statement
  //
  switch (2) {
    case 1:
      switch (1) {
        case 1:
          __builtin_print(9);
          break;
        case 2:
          __builtin_print(10);
          break;
        default:
          __builtin_print(11);
          break;
      }
      __builtin_print(12);
      break;
    case 2:
      switch (1) {
        case 1:
          __builtin_print(13);  // <- Should execute this line.
          break;
        case 2:
          __builtin_print(14);
          break;
        default:
          __builtin_print(15);
          break;
      }
      __builtin_print(16);
      break;
    default:
      switch (1) {
        case 1:
          __builtin_print(17);
          break;
        case 2:
          __builtin_print(18);
          break;
        default:
          __builtin_print(19);
          break;
      }
      __builtin_print(20);
  }
  __builtin_print(21);

  return 0;
}
