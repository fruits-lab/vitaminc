TARGET := vitaminc
CXX := g++
CC = $(CXX)
CLANG_TIDY ?= clang-tidy
CXXFLAGS = -g3 -std=c++17 -Wall -MMD -Iinclude -I$(shell llvm-config-18 --includedir) -Werror
CFLAGS = $(CXXFLAGS)
LDLIBS = -lfmt $(shell llvm-config-18 --libs core)
LEX = lex
# C++ features are used, yacc doesn't suffice
YACC = bison
# -d: generate header with default name
YFLAGS = --verbose --debug -d -Wcounterexamples -Wall
COVERAGE_DIR = $(CURDIR)/.coverage
GCOVFLAGS = -fprofile-arcs -ftest-coverage

# The Flex or Bison-generated files are not included.
SRC := $(shell find src/ -name "*.cpp") main.cpp
INC := $(shell find include/ -name "*.hpp")

OBJS := $(SRC) lex.yy.o y.tab.o
OBJS := $(OBJS:.cpp=.o)
DEPS = $(OBJS:.o=.d)

.PHONY: all clean test tidy coverage coverage-report

all: $(TARGET)

test: $(TARGET)
	$(MAKE) -C test/ test

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

#
# Using Gcov to collect coverage data and Lcov to generate HTML report.
#

coverage: CXXFLAGS += -O0 $(GCOVFLAGS)
# You might want to run coverage-report instead of coverage if you have lcov installed.
coverage: clean $(TARGET) # Make a clean build to use the coverage flags.
	@echo "[INFO] Checking if gcov is installed..."
	@command -v gcov >/dev/null 2>&1 \
		|| { echo >&2 "gcov is not installed, consider installing it first"; exit 1; }
	@echo "[INFO] Running tests..."
	@$(MAKE) test
	@echo "[INFO] Generating coverage data..."
	@gcov $(OBJS:.o=.cpp)
	@echo "[INFO] Coverage data generated."
	@echo "[INFO] Removing the executable since it's not meant to be used..."
	@$(RM) $(TARGET)

coverage-report: coverage
	@echo "[INFO] Checking if lcov is installed..."
	@command -v lcov >/dev/null 2>&1 \
		|| { echo >&2 "lcov is not installed, consider installing it first"; exit 1; }
	@# genhtml is part of lcov; no need to check for it.
	@echo "[INFO] Creating coverage directory..."
	@mkdir -p $(COVERAGE_DIR)
	@echo "[INFO] Generating coverage info..."
	@lcov --capture --directory $(CURDIR) \
		--exclude "/usr/include/*" --exclude "/usr/local/include/*" \
		--exclude "**/lex.yy.*" --exclude "**/y.tab.*" \
		--output-file $(COVERAGE_DIR)/coverage.info
	@echo "[INFO] Generating HTML report..."
	@genhtml $(COVERAGE_DIR)/coverage.info --output-directory $(COVERAGE_DIR)
	@echo "Open $(COVERAGE_DIR)/index.html in your browser to view the coverage report."

clean:
	$(RM) -r *.s *.o lex.yy.* y.tab.* *.output *.ssa *.out *.ll $(TARGET) $(OBJS) $(DEPS) \
		$(OBJS:.o=.gcda) $(OBJS:.o=.gcno) *.gcov $(COVERAGE_DIR)
	cd test/ && $(MAKE) clean

-include $(DEPS)
