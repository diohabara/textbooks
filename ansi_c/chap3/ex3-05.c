#include <stdio.h>
#define MAXLINE 1000

void itob(int n, char s[], int b);
void reverse(char s[]);

int main(int argc, char const* argv[])
{
    char s[MAXLINE] = "";
    itob(114514, s, 10);
    printf("114154 is %s in decimal\n", s);
    itob(114514, s, 16);
    printf("114514 is %s in hexadecimal\n", s);
    return 0;
}

void itob(int n, char s[], int b)
{
    int i, j, sign;
    void reverse(char s[]);

    if ((sign = n) < 0) {
        n = -n;
    }
    i = 0;
    do {
        j = n % b;
        s[i++] = (j <= 9) ? j + '0' : j - 10 + 'a';
    } while ((n /= b) > 0);
    if (sign < 0)
        s[i++] = '-';
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
