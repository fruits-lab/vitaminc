TARGET := vitaminc
CXX := g++
CXXFLAG = -g3 -std=c++14
LEX = lex
YACC = yacc
YACCFLAG = -v -t -y -d

.PHONY: all clean test

all: $(TARGET) test

test: $(TARGET)
	./$(TARGET) < test/int.c

$(TARGET): lex.yy.c y.tab.c ast.cpp
	$(CXX) $(CXXFLAG) y.tab.c -o $@

lex.yy.c: lexer.l
	$(LEX) -o $@ $^

y.tab.c: parser.y
	$(YACC) $(YACCFLAG) $^ -o $@

clean:
	rm -rf *.s *.o lex.yy.c y.tab.c y.tab.h *.output $(TARGET)
