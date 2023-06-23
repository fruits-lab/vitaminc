CXX := g++
QBE := $(shell command -v qbe 2> /dev/null)

.PHONY: all clean

all:
ifndef QBE
	$(error "qbe is not available please install qbe, https://c9x.me/compile/releases.html")
else
	qbe -o out.s hello.ssa && cc out.s -o hello.o
endif

clean:
	rm -rf *.s *.o
