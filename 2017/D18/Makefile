.PHONY: test test.watch install

# use TEST_ARGS="--match=..." to run only some tests
test:
	stack test $(STACK_TEST_ARGS) \
		--ghc-options "-j$(shell nproc)" \
		--test-arguments="+RTS -N -RTS $(TEST_ARGS)"

test.watch: STACK_TEST_ARGS="--file-watch"
test.watch: test

install:
	stack install --ghc-options "-j$(shell nproc) -O2"

run: install
	sh -c 'time ./bin/D18 data/input'

prof-space: install
	stack install --executable-profiling --library-profiling --ghc-options "-j$(shell nproc) -O2 -fprof-auto"
	./bin/D18 +RTS -M1G -xc -hc -T -RTS data/input

D18.pdf: D18.hp
	# Cannot for the life of me figure out how to invoke via stack
	~/.stack/programs/x86_64-linux/ghc-tinfo6-nopie-8.2.2/bin/hp2ps -c D18.hp
	ps2pdf D18.ps
