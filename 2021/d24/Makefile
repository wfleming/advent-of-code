CC=$(shell command -v clang++ || command -v g++)
CXXFLAGS=-std=c++20 -Wall -Wextra -Werror -I../../lib/cpp/

d24.bin: d24.cpp
	$(CC) $(CXXFLAGS) -O2 -o $@ $<

run: d24.bin
	./d24.bin input.txt

test.bin: *.cpp
	$(CC) $(CXXFLAGS) -O0 -DIS_TEST -o $@ test.cpp

test: test.bin
	./test.bin
