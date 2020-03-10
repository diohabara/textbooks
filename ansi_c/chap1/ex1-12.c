#include <stdio.h>

int main()
{
    int c;
    while ((c = getchar()) != EOF) {
        if (c == ' ' || c == '\t' || c == '\n')
            ;  // do nothing
        else{
            putchar(c);
            printf("\n");
        }
    }
    return 0;
}