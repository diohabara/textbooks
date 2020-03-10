#include <stdio.h>

#define TABSIZE 4

int main()
{
    int c, nb;

    nb = 0;

    while ((c = getchar()) != EOF) {
        if (c == ' ') {
            while ((c = getchar()) == ' ') {
                ++nb;
            }
            for (int i = 0; i < nb / TABSIZE; ++i) {
                putchar('\t');
            }
            for (int i = 0; i < nb % TABSIZE; ++i) {
                putchar(' ');
            }
        } else {
            putchar(c);
        }
    }
    return 0;
}