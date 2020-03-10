#include <stdio.h>
#include <string.h>
#define MAXLINE 1000

int main(int argc, char const* argv[])
{
    void reverse(char s[], int i, int len);
    char s[MAXLINE] = "abcde";
    reverse(s, 0, strlen(s));
    printf("%s\n", s);
    return 0;
}

void reverse(char s[], int i, int len)
{
    int c, j;
    j = len - (i + 1);
    if (i < j) {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
        reverse(s, ++i, len);
    }
}
