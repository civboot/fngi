CC=gcc
FLAGS=-v -save-temps -m32 -no-pie -g -rdynamic
DISABLE_WARNINGS=-Wno-pointer-sign -Wno-format
LIBS=-I../civc ../civc/civ/civ*
FNGI_SRC=src/fngi.* src/gen/*.c src/gen/*.h
TEST_SRC=src/tests.c
OUT=bin/tests
ARGS=

all: test

test: build
	./$(OUT) $(ARGS)

build:
	mkdir -p bin/
	../zoa/zoa_export.py src/const.zty src/gen/const
	../zoa/zoa_export.py src/spor.zty  src/gen/spor
	python3 src/gen/gen.py
	$(CC) $(FLAGS) -Wall $(DISABLE_WARNINGS) $(LIBS) $(FNGI_SRC) $(TEST_SRC) -o $(OUT)
