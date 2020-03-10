#include <stdio.h>

#define TABSIZE 4

int main()
{
    int c;

    while ((c = getchar()) != EOF) {
        if (c == '\t') {
            for (int i = 0; i < TABSIZE; ++i) {
                putchar(' ');
            }
        } else {
            putchar(c);
        }
    }
    return 0;
}