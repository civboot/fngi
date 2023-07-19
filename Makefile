CC=gcc
FLAGS=-m32 -no-pie -g -rdynamic
DISABLE_WARNINGS=-Wno-pointer-sign -Wno-format
LIBS=-Isrc/ -Igen/ -I../civc/src ../civc/src/civ*
FNGI_SRC=src/fngi.* gen/*.c gen/*.h
TEST_SRC=tests/main.c
OUT=bin/test
ARGS=

all: test

test: lua build
	./$(OUT) $(ARGS)

build:
	mkdir -p bin/
	python3 etc/gen.py
	# lua etc/gen.lua
	$(CC) $(FLAGS) -Wall $(DISABLE_WARNINGS) $(LIBS) $(FNGI_SRC) $(TEST_SRC) -o $(OUT)

lua:
	lua etc/test_civ.lua
	lua etc/test_gen.lua

dbg: build
	gdb -q ./$(OUT) $(ARGS)

