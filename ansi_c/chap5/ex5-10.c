#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUMBER 0
#define MAXVAL 100

void push(double f);
double pop(void);

int sp = 0;
double val[MAXVAL];

int main(int argc, char* argv[])
{
    int type;
    int c;
    double op1, op2, latest;
    while (--argc > 0) {
        *++argv;
        if (!isdigit(c = **argv) && strlen(*argv) == 1)
            type = c;
        else
            type = NUMBER;
        switch (type) {
        case NUMBER:
            push(atof(*argv));
            break;
        case '+':
            push(pop() + pop());
            break;
        case '*':
            push(pop() * pop());
            break;
        case '-':
            op2 = pop();
            push(pop() - op2);
            break;
        case '/':
            op2 = pop();
            if (op2 != 0.0)
                push(pop() / op2);
            else
                printf("error: zero divisor\n");
            break;
        case '%':
            op2 = pop();
            if (op2 != 0.0)
                push(fmod(pop(), op2));
            else
                printf("error: zero divisor\n");
            break;
        default:
            printf("error: unknown command: %c\n", type);
            break;
        }
    }
    latest = pop();
    printf("\t%.8g\n", latest);
    return 0;
}

void push(double f)
{
    if (sp < MAXVAL)
        val[sp++] = f;
    else
        printf("error: stack full, can't push %g\n", f);
}

double pop(void)
{
    if (sp > 0)
        return val[--sp];
    else {
        printf("error: stack empty\n");
        return 0.0;
    }
}
