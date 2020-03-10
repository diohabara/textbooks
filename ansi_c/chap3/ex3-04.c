#include <stdio.h>
#define MAXLINE 1000
#define abs(x) ((x) < 0 ? -(x) : (x))

void itoa(int n, char s[]);
void reverse(char s[]);

int main(int argc, char const* argv[])
{
    char s[MAXLINE] = "";
    itoa(114514, s);
    printf("%s\n", s);
    itoa(-114514, s);
    printf("%s\n", s);
    return 0;
}

// in a two's complement representation
// maximum positive number is -(maximum negative number) - 1
// so it cannot be converted

void itoa(int n, char s[])
{
    int i, sign;
    void reverse(char s[]);

    sign = n;
    i = 0;
    do {
        s[i++] = abs(n % 10) + '0';
    } while ((n /= 10) != 0);
    if (sign < 0) {
        s[i++] = '-';
    }
    s[i] = '\0';
    reverse(s);
}

void reverse(char s[])
{
    int i, j;
    char temp;

    i = 0;
    while (s[i] != '\0')
        ++i;
    --i;
    if (s[i] == '\n')
        --i;
    j = 0;
    while (j < i) {
        temp = s[j];
        s[j] = s[i];
        s[i] = temp;
        --i;
        ++j;
    }
}
