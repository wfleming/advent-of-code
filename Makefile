.PHONY: test install

test:
	stack test --ghc-options "-j$(shell nproc)" --test-arguments="+RTS -N -RTS"

install:
	stack install --ghc-options "-j$(shell nproc) -O2"
