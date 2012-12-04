TESTS =

.PHONY: all test clean $(TESTS)

all: test

test: $(TESTS)

clean: $(TESTS)

$(TESTS):
	$(MAKE) -C "$@" $(MAKECMDGOALS)

