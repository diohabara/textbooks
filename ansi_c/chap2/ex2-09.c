#include <stdio.h>

int bitcount(unsigned x);

int main(int argc, char const* argv[])
{
    unsigned x1 = 01010;
    unsigned x2 = x1 - 1; // 01001
    // x1&x2 = 01000
    // rightmost 1-bit is followed by 0-bits
    // since we cannot subtract 1 from 0-bits, 1-bit will be deleted
    printf("%d\n", bitcount(x1));
    return 0;
}

int bitcount(unsigned x)
{
    int b;
    for (b = 0; x != 0; x &= x - 1)
        ++b;
    return b;
}
