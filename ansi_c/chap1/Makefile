CC := gcc
CCFLAGS := -std=c11 -g -static
SUFFIXES := .c
SOURCES = $(wildcard *$(SUFFIXES))
TARGETS = $(SOURCES:.c=)

all: $(TARGETS)

.c:
	$(CC) $(CCFLAGS) -o $@ $<

clean:
	ls -a | grep -v -E '?+.c|Makefile' | xargs rm -f

.PHOHY: all

