#include <stdio.h>

unsigned rightrot(unsigned x, int n);
int wordlength(void);

int main(int argc, char const* argv[])
{
    unsigned x = 010001010;
    int n = 2;
    printf("%d %d\n", x, rightrot(x, n));
    return 0;
}

unsigned rightrot(unsigned x, int n)
{
    int wordlength(void);
    int rbit;
    while (n-- > 0) {
        rbit = (x & 1) << (wordlength() - 1);
        x >>= 1;
        x = x | rbit;
    }
    return x;
}

int wordlength(void)
{
    int i;
    unsigned v = (unsigned)~0;
    for (i = 1; (v >>= 1) > 0; ++i)
        ;
    return i;
}
