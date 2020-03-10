#include <stdio.h>
#include <string.h>
#define MAXLINE 1000

int htoi(const char s[]);
int integerize(int hex);

int main(int argc, char const* argv[])
{
    char s[MAXLINE] = "0x1bf52"; // 114514
    printf("%d is %s in hexadeciaml number\n", htoi(s), s);
    return 0;
}

int htoi(const char s[])
{
    int pos = 0;
    int res = 0;
    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        pos += 2;
    }
    for (; s[pos] != '\0'; ++pos) {
        res *= 16;
        res += integerize(s[pos]);
    }
    return res;
}

int integerize(int hex)
{
    if ('0' <= hex && hex <= '9') {
        return hex - '0';
    } else if ('a' <= hex && hex <= 'x') {
        return hex - 'a' + 10;
    } else if ('A' <= hex && hex <= 'X') {
        return hex - 'A' + 10;
    }
}
