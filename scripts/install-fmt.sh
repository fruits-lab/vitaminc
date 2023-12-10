#!/usr/bin/env sh

# number of jobs to run
NPROCS=1
# Find available processor numbers based on different OS.
OS="$(uname -s)"
case $OS in
  'Linux')
    NPROCS="$(grep -c ^processor /proc/cpuinfo)"
    ;;
  'Darwin')
    NPROCS="$(sysctl -n hw.ncpu)"
    ;;
esac

wget https://github.com/fmtlib/fmt/archive/refs/tags/10.1.1.tar.gz -O - | tar zxf - &&
  cd fmt-10.1.1/ &&
  mkdir build/ &&
  cmake -B build/ . &&
  sudo make -j"${NPROCS}" -C build/ install &&
  cd .. &&
  rm -rf fmt-10.1.1/
