EXAMPLE_SOURCES := $(filter-out %M.hs,$(wildcard *.hs))
EXAMPLE_OUTPUTS := $(EXAMPLE_SOURCES:.hs=M.hs)

.PHONY: examples examples-build
examples: $(EXAMPLE_OUTPUTS)

# Regenerate on each run so outputs reflect the current translator and runtime.
$(EXAMPLE_OUTPUTS): %M.hs: %.hs examples-build
	stack exec -- cost-translate "$<" "$@"

examples-build:
	stack build cost-translate:exe:cost-translate MemoristANT:lib
