#include <stdio.h>
#define MAXLINE 1000

int main(int argc, char const* argv[])
{
    void strncpy(char*, char*, int n);
    void strncat(char*, char*, int n);
    int strncmp(char*, char*, int n);

    char s[MAXLINE], t[MAXLINE] = "nyan";

    strncpy(s, t, 4);
    printf("%s\n", s);

    strncat(s, t, 4);
    printf("%s\n", s);

    if (strncmp(s, s, strlen(s)) == 0) {
        printf("s is equal to s\n");
    }

    return 0;
}

void strncpy(char* s, char* t, int n)
{
    while (*t && n-- > 0)
        *s++ = *t++;
    while (n-- > 0)
        *s++ = '\0';
}

void strncat(char* s, char* t, int n)
{
    void strncpy(char*, char*, int);
    int strlen(char*);

    strncpy(s + strlen(s), t, n);
}

int strncmp(char* s, char* t, int n)
{
    for (; *s == *t; s++, t++)
        if (*s == '\0' || --n <= 0)
            return 0;
    return *s - *t;
}

int strlen(char* s)
{
    char* p = s;
    while (*p != '\0')
        p++;
    return p - s;
}
