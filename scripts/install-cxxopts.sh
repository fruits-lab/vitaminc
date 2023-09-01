#!/usr/bin/env sh

wget https://github.com/jarro2783/cxxopts/archive/refs/tags/v3.1.1.tar.gz -O - | tar zxf - &&
  cd cxxopts-3.1.1/ &&
  mkdir build/ &&
  cmake -B build/ . &&
  sudo make -C build/ install
