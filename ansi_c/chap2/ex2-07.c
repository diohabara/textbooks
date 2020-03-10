#include <stdio.h>

unsigned invert(unsigned x, int p, int n);

int main(int argc, char const* argv[])
{
    unsigned x = 000011100;
    int p = 2;
    int n = 2;
    printf("%d %d\n", x, invert(x, p, n));
    return 0;
}

unsigned invert(unsigned x, int p, int n)
{
    return x ^ (~(~0 << n << (p + 1 - n)));
}
