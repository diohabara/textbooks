// Here I rewrite reverse
#include <stdio.h>

int main(int argc, char const* argv[])
{
    void reverse(char*);
    char s[] = "!!!enasni era uoY";
    reverse(s);

    printf("%s\n", s);

    return 0;
}

void reverse(char* s)
{
    int c;
    char* t;
    for (t = s + (strlen(s) - 1); s < t; s++, t--) {
        c = *s;
        *s = *t;
        *t = c;
    }
}
