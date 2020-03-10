#include <ctype.h>
#include <stdio.h>
#define BUFSIZE 100
#define SIZE 100

int getch(void);
void ungetch(int);
int getfloat(float* pn);

char buf[BUFSIZE];
int bufp = 0;

int main()
{
    int n, getfloat(float*);
    float array[SIZE];

    for (n = 0; n < SIZE && getfloat(&array[n]) != EOF; n++)
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

int getfloat(float* pn)
{
    int c, sign;
    float power;

    while (isspace(c = getch()))

        if (!isdigit(c) && c != EOF && c != '+' && c != '-' && c != '.') {
            ungetch(c);
            return 0;
        }
    sign = (c == '-') ? -1 : 1;
    if (c == '+' || c == '-')
        c = getch();
    for (*pn = 0.0; isdigit(c); c = getch()) {
        *pn = 10.0 * *pn + (c - '0');
        power *= 10.0;
    }
    *pn *= sign / power;
    if (c != EOF)
        ungetch(c);
    return c;
}
