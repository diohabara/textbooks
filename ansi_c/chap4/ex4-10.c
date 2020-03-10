#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 100
#define MAXOP 100
#define NUMBER '0'
#define NAME 'n'
#define MAXVAL 100
#define BUFSIZE 100

int getline(char line[], int limit);
int getop(char[]);
void push(double);
double pop(void);
void top(void);
void clear(void);
void mathfunc(char[]);
void ungets(char[]);

int sp = 0;
double val[MAXVAL];
int li = 0;
char line[MAXLINE];

int main(int argc, char const* argv[])
{
    int type, var = 0;
    double op1, op2, v;
    char s[MAXOP];
    double variable[26];

    for (int i = 0; i < 26; i++)
        variable[i] = 0.0;

    while ((type = getop(s)) != EOF) {
        switch (type) {
        case NUMBER:
            push(atof(s));
            break;
        case NAME:
            mathfunc(s);
            break;
        case '=':
            pop();
            if ('A' <= var && var <= 'Z')
                variable[var - 'A'] = pop();
            else
                printf("error: no variable name\n");
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
        case '%': // ex4-03
            op2 = pop();
            if (op2 != 0.0)
                push((int)pop() % (int)op2);
            else
                printf("zero divisor\n");
            break;
        // ex4-04
        case '?':
            printf("\t%.8g\n", op2 = pop());
            push(op2);
            break;
        case 'd':
            push(op2 = pop());
            push(op2);
            break;
        case 's':
            op2 = pop();
            op1 = pop();
            push(op2);
            push(op1);
            break;
        case 'c':
            clear();
            break;
        case '\n':
            printf("\t%.8g\n", v);
            break;
        default:
            if (isupper(type))
                push(variable[type - 'A']);
            else if (type == 'v')
                push(v);
            else
                printf("error: unknown command %s\n", s);
            break;
        }
        var = type;
    }
    return 0;
}

void push(double f)
{
    if (sp < MAXVAL)
        val[sp++] = f;
    else
        printf("error: stack full, can't push %g\n", f);
}

// ex4-04
void clear(void)
{
    sp = 0;
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

void mathfunc(char s[])
{
    double op2;

    if (strcmp(s, "sin") == 0)
        push(sin(pop()));
    else if (strcmp(s, "cos") == 0)
        push(cos(pop()));
    else if (strcmp(s, "exp") == 0)
        push(exp(pop()));
    else if (strcmp(s, "pow") == 0) {
        op2 = pop();
        push(pow(pop(), op2));
    } else
        printf("error: %s not supported\n", s);
}

int getop(char s[])
{
    int c, i;
    if (line[li] == '\0')
        if (getline(line, MAXLINE) == 0)
            return EOF;
        else
            li = 0;
    while ((s[0] = c = line[li++]) == ' ' || c == '\t')
        ;
    s[1] = '\0';
    if (!isdigit(c) && c != '.')
        return c;
    i = 0;
    if (isdigit(c))
        while (isdigit(s[++i] = c = line[li++]))
            ;
    s[i] = '\0';
    li--;
    return NUMBER;
}
