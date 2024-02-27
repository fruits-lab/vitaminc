int main() {
  int i = -1;
  // NOTE: The return value of main is actually enforced to be an unsigned char
  // by the C runtime.
  return +i;  // 255
}
