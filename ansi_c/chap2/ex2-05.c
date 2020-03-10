#include <stdio.h>
#define MAXLINE 1000

int any(char s1[], char s2[]);

int main(int argc, char const *argv[])
{
    char s1[MAXLINE] = "The C programming language";
    char s2[MAXLINE] = "k&r";

    printf("%s contains %s at %d\n", s1, s2, any(s1, s2));

    return 0;
}

int any(char s1[], char s2[])
{
    int i, j, k;
    for (i = j = 0; s1[i] != '\0'; ++i) {
        for (k = 0; s2[k] != '\0'; ++k) {
            if (s1[i] == s2[k]) {
                return i;
            }
        }
    }
    return -1;
}
