#!/usr/bin/env sh

wget https://github.com/jarro2783/cxxopts/archive/refs/tags/v3.1.1.tar.gz -O - | tar zxf - &&
  cd cxxopts-3.1.1/ &&
  mkdir build/ &&
  cmake -DCXXOPTS_BUILD_EXAMPLES=OFF -DCXXOPTS_BUILD_TESTS=OFF -B build/ . &&
  sudo make -C build/ install &&
  cd .. &&
  rm -rf cxxopts-3.1.1/
