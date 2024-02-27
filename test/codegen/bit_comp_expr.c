int main() {
  int i = 1;
  // The value of ~i is -2, but the C runtime converts it to an unsigned char:
  // 256 - 2 = 254
  return ~i;
}
