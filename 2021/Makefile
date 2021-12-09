.PHONY: all
all:
	(cd d01 && ./d01.rb input.txt)
	(cd d02 && ./d02.rb input.txt)
	(cd d03 && ./d03.rb input.txt)
	(cd d04 && ./d04.rb input.txt)
	(cd d05 && ./d05.rb input.txt)
	(cd d06 && ./d06.rb input.txt)
	(cd d07 && ./d07.rb input.txt)
	(cd d08 && ./d08.rb input.txt)
	(cd d09 && ./d09.rb input.txt)

CC=$(shell command -v g++ || command -v clang)
build_cpp:
	$(CC) -std=c++17 -Wextra -Wall -Werror -o d01/d01.bin d01/d01.cpp

run_cpp: build_cpp
	d01/d01.bin d01/input.txt
