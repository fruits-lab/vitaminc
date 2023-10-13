TARGET := vitaminc
CXX := g++
CC = $(CXX)
CXXFLAGS = -g3 -std=c++14 -Wall -MMD -Iinclude -Werror
CFLAGS = $(CXXFLAGS)
LEX = lex
# C++ features are used, yacc doesn't suffice
YACC = bison
# -d: generate header with default name
YFLAGS = --verbose --debug -d

# Note that lex.yy.cpp is excluded deliberately, as "lex.yy.cpp" is considered a
# header file (it's included by "y.tab.cpp").
OBJS := $(shell find . -name "*.cpp" ! -name "y.tab.cpp" ! -name "lex.yy.cpp" ) y.tab.o
OBJS := $(OBJS:.cpp=.o)
DEPS = $(OBJS:.o=.d)

.PHONY: all clean test

all: $(TARGET) test

test: $(TARGET)
	make -C test/

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) -o $@

lex.yy.cpp: lexer.l
	$(LEX) -o $@ $<

y.tab.hpp: y.tab.cpp
y.tab.cpp: parser.y lex.yy.cpp
	$(YACC) $(YFLAGS) $< -o $@

#
# Please note that although we're handling dependencies automatically with -MMD,
# files that includes Flex or Bison-generated files still have to make such
# dependency explicit to enforce the ordering.
#

main.o: %.o: %.cpp y.tab.hpp

clean:
	rm -rf *.s *.o lex.yy.* y.tab.* *.output *.ssa $(TARGET) $(OBJS) $(DEPS)
	make -C test/ clean

-include $(DEPS)
