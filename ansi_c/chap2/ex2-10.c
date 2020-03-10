#include <stdio.h>
#define MAXLINE 1000

int lower(int c);

int main(int argc, char const *argv[])
{
    char c = 'A';
    printf("%c is %c in lower case\n", c, lower(c));
    return 0;
}

int lower(int c)
{
    return ('A' <= c && c <= 'Z') ? c + 'a' - 'A' : 0;
}
