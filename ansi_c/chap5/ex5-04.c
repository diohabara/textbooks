#include <stdio.h>

int main(int argc, char const* argv[])
{
    char s[] = "foobar", t[] = "bar";
    int strend(char*, char*);
    if (strend(s, t))
        printf("You did it!\n");
    return 0;
}

int strend(char* s, char* t)
{
    char* bs = s;
    char* bt = t;

    for (; *s; s++)
        ;
    for (; *t; t++)
        ;
    for (; *s == *t; s--, t--)
        if (t == bt || s == bs)
            break;
    if (*s == *t && t == bt && *s != '\0')
        return 1;
    else
        return 0;
}
