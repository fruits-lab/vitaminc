#!/usr/bin/env sh

wget https://c9x.me/compile/release/qbe-1.1.tar.xz -O - | tar Jxf - &&
  cd qbe-1.1/ &&
  make &&
  sudo make install &&
  cd .. &&
  rm -rf qbe-1.1/
