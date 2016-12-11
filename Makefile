.PHONY: test install

test:
	stack test --test-arguments="+RTS -N -RTS"

install:
	stack install --ghc-options "-O2"
