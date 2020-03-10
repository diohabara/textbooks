#include <stdio.h>
#include <limits.h>
#include <float.h>

int main(int argc, char const *argv[])
{
    // signed
    printf("Here the ranges of signed types\n");
    printf("The range of char is from %d to %d\n", SCHAR_MIN, SCHAR_MAX);
    printf("The range of short is from %d to %d\n", SHRT_MIN, SHRT_MAX);
    printf("The range of int is from %d to %d\n", INT_MIN, INT_MAX);
    printf("The range of long is from %ld to %ld\n", LONG_MIN, LONG_MAX);

    // unsigned
    printf("Here the ranges of unsigned types\n");
    printf("The upper of char is %u\n", UCHAR_MAX);
    printf("The upper of short is %u\n", USHRT_MAX);
    printf("The upper of int is %u\n", UINT_MAX);
    printf("The upper of long is %lu\n", ULONG_MAX);
    return 0;
}
