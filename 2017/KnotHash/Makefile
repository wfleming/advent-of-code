.PHONY: test test.watch

# use TEST_ARGS="--match=..." to run only some tests
test:
	stack test $(STACK_TEST_ARGS) \
		--ghc-options "-j$(shell nproc)" \
		--test-arguments="+RTS -N -RTS $(TEST_ARGS)"

test.watch: STACK_TEST_ARGS="--file-watch"
test.watch: test
