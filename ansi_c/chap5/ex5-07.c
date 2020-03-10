#include <stdio.h>
#include <string.h>

#define MAXLINES 5000
#define MAXLEN 1000
#define MAXSTOR 5000
#define ALLOCSIZE 10000

static char allocbuf[ALLOCSIZE];
static char* allocp = allocbuf;
char* lineptr[MAXLINES];
char linestor[MAXSTOR];

int readlines(char* lineptr[], char* linestor, int nlines);
void writelines(char* lineptr[], int nlines);
void qsort(char* lineptr[], int left, int right);
int getline(char*, int);
char* alloc(int);

int main(void)
{
    int nlines;

    if ((nlines = readlines(lineptr, linestor, MAXLINES)) >= 0) {
        qsort(lineptr, 0, nlines - 1);
        writelines(lineptr, nlines);
        return 0;
    } else {
        printf("error: input too big to sort\n");
        return 1;
    }

    return 0;
}

int readlines(char* lineptr[], char* linestor, int maxlines)
{
    int len, nlines;
    char line[MAXLEN];
    char* p = linestor;
    char* linestop = linestor + MAXSTOR;

    nlines = 0;
    while ((len = getline(line, MAXLEN)) > 0)
        if (nlines >= maxlines || p + len > linestop)
            return -1;
        else {
            line[len - 1] = '\0';
            strcpy(p, line);
            lineptr[nlines++] = p;
            p += len;
        }
    return nlines;
}

void writelines(char* lineptr[], int nlines)
{
    while (nlines-- > 0)
        printf("%s\n", *lineptr++);
}

void qsort(char* v[], int left, int right)
{
    int i, last;
    void swap(char* v[], int i, int j);

    if (left >= right)
        return;
    swap(v, left, (left + right) / 2);
    last = left;
    for (i = left + 1; i <= right; i++)
        if (strcmp(v[i], v[left]) < 0)
            swap(v, ++last, i);
    swap(v, left, last);
    qsort(v, left, last - 1);
    qsort(v, last + 1, right);
}

int getline(char s[], int lim)
{
    int c, i;

    i = 0;
    while (--lim > 0 && (c = getchar() != EOF && c != '\n'))
        s[i++] = c;
    if (c == '\n')
        s[i++] = c;
    s[i] = '\0';
    return i;
}

char* alloc(int n)
{
    if (allocbuf + ALLOCSIZE - allocp >= n) {
        allocp += n;
        return allocp - n;
    } else
        return 0;
}

void swap(char* v[], int i, int j)
{
    char* temp;

    temp = v[i];
    v[i] = v[j];
    v[j] = temp;
}
