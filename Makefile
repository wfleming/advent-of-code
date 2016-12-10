.PHONY: test

test:
	stack test --test-arguments="+RTS -N -RTS"
