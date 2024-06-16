.PHONY: test clean

test:
	@# NOTE: The -j option is added to MAKEFLAGS only when entering the recipe.
	@# Therefore, we have to check it here.
	@case "$(MAKEFLAGS)" in \
		*j*) \
			turnt **/*.c --diff -j; \
			;; \
		*) \
			turnt **/*.c --diff; \
			;; \
	esac


clean:
	rm -f *.s **/*.s *.o **/*.o *.ssa **/*.ssa **/*.ll
