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
SRC_FILES+=src/cfind.c

LINKED_LIBS=-L/usr/local/lib
LINKED_LIBS+=-lcjson


cfind:
	$(CC) $(CFLAGS) $(INCLUDE) $(SRC_FILES) $(LINKED_LIBS) -o cfind

all: cfind

clean:
	rm -f cfind
