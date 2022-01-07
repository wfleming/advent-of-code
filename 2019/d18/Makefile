CC=$(shell command -v g++ || command -v clang++)
CXXFLAGS=-std=c++20 -Wall -Wextra -Werror -I../../lib/cpp/

.PHONY: watch
watch:
	watchexec --exts cpp make test

spec.bin: *.cpp
	$(CC) $(CXXFLAGS) -DIS_TEST -o $@ spec.cpp

test: spec.bin
	./spec.bin --order=rand

d18.bin: d18.cpp
	$(CC) $(CXXFLAGS) -O2 -o $@ d18.cpp

.PHONY: run
run: d18.bin
	./d18.bin input.txt
