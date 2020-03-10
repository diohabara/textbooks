#include <stdio.h>
#define MAXLINE 1000

void squeeze(char s1[], char s2[]);

int main(int argc, char const* argv[])
{
    char s1[MAXLINE] = "You are truely genius.";
    char s2[MAXLINE] = "eigo";
    printf("When you remove %s from %s, you get", s2, s1);
    squeeze(s1, s2);
    printf(" %s\n", s1);
    return 0;
}

void squeeze(char s1[], char s2[])
{
    int i, j, k;
    for (i = k = 0; s1[i] != '\0'; ++i) {
        for (j = 0; s2[j] != '\0' && s2[j] != s1[i]; ++j)
            ;
        if (s2[j] == '\0') {
            s1[k] = s1[i];
            ++k;
        }
    }
    s1[k] = '\0';
}
