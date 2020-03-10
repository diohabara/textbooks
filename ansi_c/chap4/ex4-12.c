#include <math.h>
#include <stdio.h>
#define MAXLINE 1000

int main(int argc, char const* argv[])
{
    void itoa(int, char[]);
    int n = 123456;
    char s[MAXLINE];
    itoa(n, s);
    printf("%s\n", s);
    return 0;
}

void itoa(int n, char s[])
{
    static int i;
    if (n / 10)
        itoa(n / 10, s);
    else {
        i = 0;
        if (n < 0)
            s[i++] = '-';
    }
    s[i++] = abs(n) % 10 + '0';
    s[i] = '\0';
}
