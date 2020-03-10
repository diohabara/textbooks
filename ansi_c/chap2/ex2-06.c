#include <stdio.h>

unsigned setbits(unsigned x, unsigned p, unsigned n, unsigned y);

int main(int argc, char const* argv[])
{
    unsigned x = 000011100;
    unsigned p = 2;
    unsigned n = 2;
    unsigned y = 000000011;
    printf("%d\n", setbits(x, p, n, y));
    return 0;
}

unsigned setbits(unsigned x, unsigned p, unsigned n, unsigned y)
{
    return x & ~(~(~0 << n) << (p - (n - 1))) | (y & ~(~0 << n)) << (p - (n - 1));
}
