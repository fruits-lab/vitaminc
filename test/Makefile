.PHONY: test clean

test:
	turnt *.c **/*.c --diff
	qbe -o out.s test.ssa && cc out.s -o a.o && ./a.o

clean:
	rm -f *.s *o *.ssa
