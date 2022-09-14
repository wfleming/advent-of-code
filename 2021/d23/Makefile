CC=$(shell command -v clang++ || command -v g++)
CXXFLAGS=-std=c++20 -Wall -Wextra -Werror -I../../lib/cpp/

d23.bin: d23.cpp d23.hpp
	$(CC) $(CXXFLAGS) -O0 -g3 -o $@ $<
