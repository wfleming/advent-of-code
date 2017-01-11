.PHONY: test install

# use TEST_ARGS="--match=..." to run only some tests
test:
	stack test --ghc-options "-j$(shell nproc)" --test-arguments="+RTS -N -RTS $(TEST_ARGS)"

install:
	stack install --ghc-options "-j$(shell nproc) -O2"
