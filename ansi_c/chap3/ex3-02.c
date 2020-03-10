#include <stdio.h>

void escape(char s[], char t[]);
void unescape(char s[], char t[]);

int main(int argc, char const* argv[])
{
    char s[] = "You can't see me";
    char t[] = "foo\tbar\n";
    escape(s, t);
    printf("%s\n", s);

    char s2[] = "You cannot see me";
    char t2[] = "foo\\tbar\\n";
    unescape(s2, t2);
    printf("%s\n", s2);
    return 0;
}

void escape(char s[], char t[])
{
    int i, j;
    for (i = j = 0; t[i] != '\0'; ++i)
        switch (t[i]) {
        case ('\n'):
            s[j++] = '\\';
            s[j++] = 'n';
            break;
        case ('\t'):
            s[j++] = '\\';
            s[j++] = 't';
            break;
        default:
            s[j++] = t[i];
            break;
        }
    s[j] = '\0';
}

void unescape(char s[], char t[])
{
    int i, j;

    for (i = j = 0; t[i] != '\0'; ++i) {
        if (t[i] != '\\') {
            s[j] = t[i];
            ++j;
        } else {
            switch (t[++i]) {
            case 'n':
                s[j++] = '\n';
                break;
            case 't':
                s[j++] = '\t';
                break;
            default:
                s[j++] = '\\';
                s[j++] = t[i];
                break;
            }
        }
    }
    s[j] = '\0';
}
