int main() {
  int i = 10;
  // NOTE: According to QBE requirements: "Unless the called function does not
  // return a value, a return temporary must be specified, even if it is never
  // used afterwards." However, since we typically don't use the return value
  // of `printf`, and `__builtin_print` cannot return `void` as this type is not
  // yet supported, the return value of `__builtin_print` must be handled
  // manually.
  int _ = __builtin_print(i);
  return 0;
}
