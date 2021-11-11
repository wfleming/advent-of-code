.PHONY: all
all:
	(cd d01 && nim compile --out=d01.bin --run d01.nim input.txt)
	(cd d02 && nim compile --out=d02.bin --run d02.nim input.txt)
	(cd d03 && nim compile --out=d03.bin --run d03.nim input.txt)
	(cd d04 && nim compile --d:release --opt:speed --threads:on --out=d04.bin --run d04.nim input.txt)
	(cd d05 && nim compile --out=d05.bin --run d05.nim input.txt)
	(cd d06 && nim compile --out=d06.bin --run d06.nim input.txt)
