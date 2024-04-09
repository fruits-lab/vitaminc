#ifndef LOCATION_HPP_
#define LOCATION_HPP_

#include <ostream>

struct Location {
  int line;
  int column;
};

/// @brief Outputs the location in the format "line:column".
inline std::ostream& operator<<(std::ostream& os, const Location& loc) {
  return os << loc.line << ":" << loc.column;
  ;
}

#endif /* LOCATION_HPP_ */
