CC=$(shell command -v clang++ || command -v g++)
CXXFLAGS=-std=c++20 -Wall -Wextra -Werror -I../../lib/cpp/

d25.bin: d25.cpp
	$(CC) $(CXXFLAGS) -O2 -o $@ $<

run: d25.bin
	./d25.bin input.txt

test.bin: *.cpp
	$(CC) $(CXXFLAGS) -O0 -DIS_TEST -o $@ test.cpp

test: test.bin
	./test.bin
