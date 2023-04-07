.PHONY: default
default: build_cfind;

CC=cc
CFLAGS=-Wall
# CFLAGS+=-Werror
CFLAGS+=-g
CFLAGS+=-pedantic
CFLAGS+=-std=iso9899:1999
CFLAGS+=-Wshadow
CFLAGS+=-Wpointer-arith
# CFLAGS+=-Wcast-qual
CFLAGS+=-Wcast-align
CFLAGS+=-Wstrict-prototypes
CFLAGS+=-Wmissing-prototypes
CFLAGS+=-Wconversion
#CFLAGS+=-Wl,-rpath=/usr/lib

INCLUDE=-I./include
INCLUDE+=-I/usr/local/include/cjson

SRC_FILES=src/common.c
SRC_FILES+=src/config.c
SRC_FILES+=src/finderr.c
SRC_FILES+=src/regexnode.c
SRC_FILES+=src/stringarray.c
SRC_FILES+=src/intnode.c
SRC_FILES+=src/stringnode.c
SRC_FILES+=src/fileresults.c
SRC_FILES+=src/filetypes.c
SRC_FILES+=src/fileutil.c
SRC_FILES+=src/findoptions.c
SRC_FILES+=src/findsettings.c
SRC_FILES+=src/finder.c

SRC_EXE=src/cfind.c

TEST_INCLUDE=-I./test

TEST_FILES=test/test_common.c
TEST_FILES+=test/test_filetypes.c
TEST_FILES+=test/test_fileutil.c
TEST_FILES+=test/test_finder.c
TEST_FILES+=test/test_findoptions.c
TEST_FILES+=test/test_findsettings.c

TEST_EXE=test/test_cfind.c

LINKED_LIBS=-L/usr/local/lib
LINKED_LIBS+=-lcjson
#LINKED_LIBS+=-lpcre2-8


build_cfind:
	$(CC) $(CFLAGS) $(INCLUDE) $(SRC_FILES) $(SRC_EXE) $(LINKED_LIBS) -o cfind

build_tests:
	$(CC) $(CFLAGS) $(INCLUDE) $(TEST_INCLUDE) $(SRC_FILES) $(TEST_FILES) $(TEST_EXE) $(LINKED_LIBS) -o test/test_cfind

all: build_cfind build_tests

clean_cfind:
	rm -f cfind
	rm -rf cfind.dSYM

clean_test:
	rm -f test/test_cfind
	rm -rf test/test_cfind.dSYM

clean: clean_cfind clean_test

run_tests: build_tests
	./test/test_cfind
