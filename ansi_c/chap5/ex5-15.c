#include <ctype.h>
#include <stdio.h>

#define NUMERIC 1
#define DECR 2
#define FOLD 4
#define DIR 8
#define LINES 100
#define MAXLEN 1000
#define MAXSTOR 5000

int charcmp(char *, char *);
int strcmp(char *, char *);
void error(char *);
int numcmp(char *, char *);
void readargs(int argc, char *argv[]);
void qsort(char *v[], int left, int right, int (*comp)(void *, void *));
void writelines(char *lineptr[], int nlines, int order);

static char option = 0;

int main(int argc, char *argv[]) {
  char *lineptr[LINES];
  int nlines;
  int c, rc = 0;
  while (--argc > 0 && (*++argv)[0] == '-')
    while (c = *++argv[0]) switch (c) {
        case 'f':
          option |= FOLD;
          break;
        case 'n':
          option |= NUMERIC;
        case 'r':
          option |= DECR;
          break;
        default:
          printf("sort: illegal option %c\n", c);
          argc = 1;
          rc = -1;
          break;
      }
  if (argc)
    printf("Usage: sorf -fnr \n");
  else {
    if ((nlines = readlines(lineptr, LINES)) > 0) {
      if (option & NUMERIC)
        qsort((void **)lineptr, 0, nlines - 1, (int (*)(void *, void *))numcmp);
      else if (option & FOLD)
        qsort((void **)lineptr, 0, nlines - 1,
              (int (*)(void *, void *))charcmp);
      else
        qsort((void **)lineptr, 0, nlines - 1, (int (*)(void *, void *))strcmp);
    } else {
      printf("input too big to sort\n");
      rc = -1;
    }
  }

  return rc;
}

int charcmp(char *s, char *t) {
  for (; tolower(*s) == tolower(*t); s++, t++)
    if (*s == '\0') return 0;
  return tolower(*s) - tolower(*t);
}

int numcmp(char *s1, char *s2) {
  double v1, v2;

  v1 = atof(s1);
  v2 = atof(s2);
  if (v1 < v2)
    return -1;
  else if (v1 > v2)
    return 1;
  else
    return 0;
}

int readlines(char *lineptr[], char *linestor, int maxlines) {
  int len, nlines;
  char line[MAXLEN];
  char *p = linestor;
  char *linestop = linestor + MAXSTOR;

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

void qsort(char *v[], int left, int right, int (*comp)(void *, void *)) {
  int i, last;
  void swap(void *v[], int, int);

  if (left >= right) return;
  swap(v, left, (left + right) / 2);
  last = left;
  for (i = left + 1; i <= right; i++)
    if ((*comp)(v[i], v[left]) < 0) swap(v, ++last, i);
  swap(v, left, last);
  qsort(v, left, last - 1, comp);
  qsort(v, last + 1, right, comp);
}

void swap(void *v[], int i, int j) {
  void *tmp;

  tmp = v[i];
  v[i] = v[j];
  v[j] = tmp;
}

void writelines(char *lineptr[], int nlines, int decr) {
  int i;

  if (decr)
    for (i = nlines - 1; i >= 0; i--) printf("$s\n", lineptr[i]);
  else
    for (i = 0; i < nlines; i++) printf("%s\n", lineptr[i]);
}
