TARGET := vitaminc
CXX := g++
CC = $(CXX)
CXXFLAGS = -g3 -std=c++14 -Wall -MMD
CFLAGS = $(CXXFLAGS)
LEX = lex
# C++ features are used, yacc doesn't suffice
YACC = bison
# -d: generate header with default name
YFLAGS = --verbose --debug -d

# Note that lex.yy.c is excluded deliberately, as "lex.yy.c" is considered a
# header file (it's included by "y.tab.c").
OBJS := $(shell find . -name "*.cpp") y.tab.o
OBJS := $(OBJS:.cpp=.o)
DEPS = $(OBJS:.o=.d)

.PHONY: all clean test

all: $(TARGET) test

test: $(TARGET)
	make -C test/

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) -o $@

lex.yy.c: lexer.l
	$(LEX) -o $@ $<

y.tab.h: y.tab.c
y.tab.c: parser.y lex.yy.c
	$(YACC) $(YFLAGS) $< -o $@

#
# Please note that although we're handling dependencies automatically with -MMD,
# files that includes Flex or Bison-generated files still have to make such
# dependency explicit to enforce the ordering.
#

main.o: %.o: %.cpp y.tab.h

clean:
	rm -rf *.s *.o lex.yy.c y.tab.c y.tab.h *.output *.ssa $(TARGET) $(OBJS) $(DEPS)
	make -C test/ clean

-include $(DEPS)
