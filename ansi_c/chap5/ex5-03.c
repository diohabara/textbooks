#include <stdio.h>

int main(int argc, char const* argv[])
{
    char s[] = "foo", t[] = "bar";
    void strcat(char*, char*);
    strcat(s, t);
    printf("%s\n", s);
    return 0;
}

void strcat(char* s, char* t)
{
    while (*s)
        s++;
    while (*s++ = *t++)
        ;
}
