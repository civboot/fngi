CC=gcc
FLAGS=-m32 -no-pie -g -rdynamic
DISABLE_WARNINGS=-Wno-pointer-sign -Wno-format
LIBS=-Isrc/ -Igen/ -I../civc/src ../civc/src/civ*
FNGI_SRC=src/fngi.* gen/*.c gen/*.h
TEST_SRC=tests/main.c
OUT=bin/test
ARGS=

all: test

test: build
	./$(OUT) $(ARGS)

build:
	mkdir -p bin/
	../zoa/zoa_export.py src/const.zty gen/const
	../zoa/zoa_export.py src/spor.zty  gen/spor
	python3 etc/gen.py
	$(CC) $(FLAGS) -Wall $(DISABLE_WARNINGS) $(LIBS) $(FNGI_SRC) $(TEST_SRC) -o $(OUT)
