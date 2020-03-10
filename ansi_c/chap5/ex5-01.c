#include <ctype.h>
#include <stdio.h>
#define BUFSIZE 100
#define SIZE 100

int getch(void);
void ungetch(int);
int getint(int* pn);

char buf[BUFSIZE];
int bufp = 0;

int main()
{
    int n, array[SIZE], getint(int*);

    for (n = 0; n < SIZE && getint(&array[n]) != EOF; n++)
        ;

    return 0;
}

int getch(void)
{
    return (bufp > 0) ? buf[--bufp] : getchar();
}

void ungetch(int c)
{
    if (bufp >= BUFSIZE)
        printf("ungetch: too many characters\n");
    else
        buf[bufp++] = c;
}

int getint(int* pn)
{
    int c, d, sign;

    while (isspace(c = getch()))
        ;
    if (!isdigit(c) && c != EOF && c != '+' && c != '-') {
        ungetch(c);
        return 0;
    }
    sign = (c == '-') ? -1 : 1;
    if (c == '+' || c == '-') {
        d = c;
        if (!isdigit(c = getch())) {
            if (c != EOF)
                ungetch(c);
            ungetch(d);
            return d;
        }
    }
    for (*pn = 0; isdigit(c); c = getch())
        *pn = 10 * *pn + (c - '0');
    *pn += sign;
    if (c != EOF)
        ungetch(c);
    return c;
}
