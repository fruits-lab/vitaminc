TARGET := vitaminc
CXX := g++
CC = $(CXX)
CLANG_TIDY ?= clang-tidy
CXXFLAGS = -g3 -std=c++17 -Wall -MMD -Iinclude -Werror
CFLAGS = $(CXXFLAGS)
LDLIBS = -lfmt
LEX = lex
# C++ features are used, yacc doesn't suffice
YACC = bison
# -d: generate header with default name
YFLAGS = --verbose --debug -d -Wcounterexamples -Wall
# Check if -j multiple jobs is configured.
PARALLEL = false
ifneq (,$(findstring j,$(MAKEFLAGS)))
	PARALLEL := true
endif
# Export variable to be visible for test/Makefile.
export PARALLEL

# The Flex or Bison-generated files are not included.
SRC := $(shell find src/ -name "*.cpp") main.cpp
INC := $(shell find include/ -name "*.hpp")

OBJS := $(SRC) lex.yy.o y.tab.o
OBJS := $(OBJS:.cpp=.o)
DEPS = $(OBJS:.o=.d)

.PHONY: all clean test tidy

all: $(TARGET)

test: $(TARGET)
	# Flags are omitted to prevent passing the -j option since Turnt, our testing tool,
	# has its own mechanism for parallel builds.
	cd test/ && $(MAKE) test MAKEFLAGS=

$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) -o $@ $(LDLIBS)

lex.yy.cpp: lexer.l y.tab.hpp
	$(LEX) -o $@ $<

y.tab.hpp: y.tab.cpp
y.tab.cpp: parser.y
	$(YACC) $(YFLAGS) $< -o $@

#
# Please note that although we're handling dependencies automatically with -MMD,
# files that includes Flex or Bison-generated files still have to make such
# dependency explicit to enforce the ordering.
#

main.o: %.o: %.cpp y.tab.hpp

# Since y.tab.hpp is included by the source files, it must exist;
# otherwise, a clang-diagnostic-error will be raised.

tidy: y.tab.hpp
	$(CLANG_TIDY) $(CLANG_TIDY_FLAGS) -p . $(SRC) $(INC) -- $(CXXFLAGS)

clean:
	rm -rf *.s *.o lex.yy.* y.tab.* *.output *.ssa $(TARGET) $(OBJS) $(DEPS)
	cd test/ && $(MAKE) clean

-include $(DEPS)
