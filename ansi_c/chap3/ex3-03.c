#include <stdio.h>
#define MAXLINE 1000

void expand(char s1[], char s2[]);

int main(int argc, char const* argv[])
{
    char s1[MAXLINE] = "a-z";
    char s2[MAXLINE] = "";
    expand(s1, s2);
    printf("%s\n", s2);
    return 0;
}

void expand(char s1[], char s2[])
{
    char c;
    int i, j;
    while ((c = s1[i++]) != '\0') {
        if (s1[i] == '-' && s1[i + 1] >= c) {
            i++;
            while (c < s1[i]) {
                s2[j++] = c++;
            }
        } else {
            s2[j++] = c;
        }
    }
    s2[j] = '\0';
}
