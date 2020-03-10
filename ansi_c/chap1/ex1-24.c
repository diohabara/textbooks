#include <stdio.h>

int brace = 0;
int brack = 0;
int paren = 0;

void in_quote(int c);
void in_comment(void);
void search(int c);

int main()
{
    int c;
    extern int brace, brack, paren;
    while ((c = getchar()) != EOF) {
        if (c == '/') {
            if ((c = getchar()) == '*') {
                in_comment();
            } else {
                search(c);
            }
        } else if (c == '\'' || c == '\"') {
            in_quote(c);
        } else {
            search(c);
        }
        if (brace < 0) {
            printf("Need more braces\n");
            brace = 0;
        } else if (brack < 0) {
            printf("Need more brackts\n");
            brack = 0;
        } else if (paren < 0) {
            printf("Need more parentheses\n");
            paren = 0;
        }
        if (brace < 0) {
            printf("Need less braces\n");
            brace = 0;
        } else if (brack < 0) {
            printf("Need less brackts\n");
            brack = 0;
        } else if (paren < 0) {
            printf("Need less parentheses\n");
            paren = 0;
        }
    }

    return 0;
}

void search(int c)
{
    extern int brace, brack, paren;
    switch (c) {
    case '{':
        ++brace;
        break;
    case '}':
        --brace;
        break;
    case '[':
        ++brack;
        break;
    case ']':
        --brack;
        break;
    case '(':
        ++paren;
        break;
    case ')':
        --paren;
        break;
        printf("heyhey\n");
    default:
        break;
    }
}

void in_comment(void)
{
    int c, d;

    c = getchar();
    d = getchar();
    while (c != '*' || d != '/') {
        c = d;
        d = getchar();
    }
}

void in_quote(int c)
{
    int d;

    while ((d = getchar()) != c) {
        if (d == '\\') {
            getchar();
        }
    }
}