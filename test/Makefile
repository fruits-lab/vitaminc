.PHONY: test-all test-typecheck test-qbe test-llvm clean

TURNTFLAGS = --diff


# NOTE: Not placing individual test targets as the dependencies so that the tests
# are not aborted if any of the individual tests fail.
test-all:
	@# NOTE: The -j option is added to MAKEFLAGS only when entering the recipe.
	@# Therefore, we have to check it here.
	@case "$(MAKEFLAGS)" in \
		*j*) \
			turnt **/*.c $(TURNTFLAGS) -j; \
			;; \
		*) \
			turnt **/*.c $(TURNTFLAGS); \
			;; \
	esac

test-typecheck:
	@case "$(MAKEFLAGS)" in \
		*j*) \
			turnt typecheck/*.c $(TURNTFLAGS) -j; \
			;; \
		*) \
			turnt typecheck/*.c $(TURNTFLAGS); \
			;; \
	esac

test-qbe:
	@case "$(MAKEFLAGS)" in \
		*j*) \
			turnt codegen/*.c --env qbe $(TURNTFLAGS) -j; \
			;; \
		*) \
			turnt codegen/*.c --env qbe $(TURNTFLAGS); \
			;; \
	esac

test-llvm:
	@case "$(MAKEFLAGS)" in \
		*j*) \
			turnt codegen/*.c --env llvm $(TURNTFLAGS) -j; \
			;; \
		*) \
			turnt codegen/*.c --env llvm $(TURNTFLAGS); \
			;; \
	esac


clean:
	$(RM) *.s **/*.s *.o **/*.o *.ssa **/*.ssa **/*.ll
