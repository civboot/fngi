CC=gcc
FLAGS=-m32
DISABLE_WARNINGS=-Wno-pointer-sign -Wno-format
LIBS=-I../civc ../civc/civ/civ*
FNGI_SRC=src/fngi.*
TEST_SRC=src/tests.c
OUT=bin/a.out

all: test

test: build
	./bin/a.out

build:
	mkdir -p bin/
	../zoa/zoa_export.py src/const.zty src/gen/const
	../zoa/zoa_export.py src/spor.zty  src/gen/spor
	$(CC) $(FLAGS) -Wall $(DISABLE_WARNINGS) $(LIBS) $(FNGI_SRC) $(TEST_SRC) -o $(OUT)
