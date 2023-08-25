TARGET := vitaminc
CXX := g++
CXXFLAG = -g3 -std=c++14 -Wall
LEX = lex
# C++ features are used, yacc doesn't suffice
YACC = bison
# -d: generate header with default name
YACCFLAG = --verbose --debug -d

.PHONY: all clean test

all: $(TARGET) test

test: $(TARGET)
	make -C test/

$(TARGET): main.cpp lex.yy.c y.tab.c ast.hpp
	$(CXX) $(CXXFLAG) $< y.tab.c -o $@

lex.yy.c: lexer.l
	$(LEX) -o $@ $^

y.tab.c: parser.y
	$(YACC) $(YACCFLAG) $^ -o $@

clean:
	rm -rf *.s *.o lex.yy.c y.tab.c y.tab.h *.output *.ssa $(TARGET)
	make -C test/ clean
