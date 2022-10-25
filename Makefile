
all: test

test: build
	echo "Testing civ_h"
	./bin/civ_h
	echo "Testing fngi boot"
	./bin/fngi_boot

build:
	python3 etc/make.py --build
	mkdir -p bin/
	gcc -m32 civ/*.c -o bin/civ_h
	gcc -m32 civ/civ*.c boot/*.c -o bin/fngi_boot
