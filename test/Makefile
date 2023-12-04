TURNTFLAGS = --diff
ifeq (true, $(PARALLEL))
	TURNTFLAGS += -j
endif

.PHONY: test clean

test:
	turnt **/*.c $(TURNTFLAGS)

clean:
	rm -f *.s **/*.s *.o **/*.o *.ssa **/*.ssa
