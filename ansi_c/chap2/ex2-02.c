#include <stdio.h>
#define MAXLINE 1000

int main(int argc, char const* argv[])
{
    int lim = 1000;
    char s[MAXLINE];

    for (int i = 0; i < lim - 1; ++i) {
        int c;
        if ((c = getchar()) != '\n')
            if (c != EOF)
                s[i] = c;
            else
                break;
        else
            break;
    }
    printf("%s\n", s);
    return 0;
}
