#include <stdio.h>

#define MAXCOL 10
#define TABSIZE 4

char line[MAXCOL];

int main()
{
    int c, pos;
    pos = 0;
    while ((c = getchar()) != EOF) {
        if (c == '\t') {
            for (int i = 0; i < TABSIZE; ++i)
                putchar(' ');
            ++pos;
        } else if (c == '\n') {
            putchar(c);
            pos = 0;
        } else if (++pos >= MAXCOL) {
            putchar(c);
            putchar('\n');
            pos = 0;
        } else {
            putchar(c);
        }
    }
}