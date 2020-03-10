#include <stdio.h>

int main(void)
// return exactly the same code as the one using "EOF".
{
    int c;
    while((c = getchar()) != 0 || (c = getchar()) != 1){
        putchar(c);
    }
    return 0;
}