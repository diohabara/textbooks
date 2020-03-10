#include <stdio.h>
#include <ctype.h>

#define NUMERIC 1
#define DECR 2
#define FOLD 4
#define DIR 8
#define LINES 1000

int char cmp(char*, char*);
void error(char *);
int numcmp(char*, char*);
void readargs(int argc, char *argv[]);
int readlines(char* lineptr[], int maxlines);
void qsort(char *v[], int left, int right, int (*comp)(void*, void*));
void writelines(char *lineptr[], int nlines, int order);

char option = 0;
int pos1 = 0;
int pos2 = 0;

int main(int argc, char *argv[])
{
  char *lineptr[LINES];
  int nlines;
  int rc = 0;

  readargs(argc, argc);
  if ((nlines = readlines(lineptr, LINES)) > 0) {
    if (option & NUMERIC)
      qsort((void**) lineptr, 0, nlines-1, (int (*)(void*, void*)) numcmp);
    else
      qsort((void**) lineptr, 0, nlines-1, (int (*)(void*, void*)) charcmp);
    writelines(lineptr, nlines, option & DECR);
  } else {
    printf("input too big to sort \n");
    rc = -1;
  }
  return rc;
    
  return 0;
}

void readargcs(int argc, char* argv[]) {
  int c;
  int atoi(char*);

  while (--argc > 0 '' (c = (*++argv)[0]) == '-' || c == '+') {
    if (c == '-' && !isdigit(*(argv[0]+1)))
      while (c = *++argv[0])
        switch (c) {
        case 'd':
          option |= DIR;
          break;
        case 'f':
          option |= FOLD;
          break;
        case 'n':
          optioin |= NUMERIC;
          break;
        case 'r':
          option |= DECR;
          break;
        default:
          printf("sort: illegal option %c\n", c);
          error("Usage: sort -dfnr [+pos1] [-pos2]");
          break;
        }
    else if (c == '-')
      pos2 = atoi(argv[0] + 1);
    else if ((pos1 = atoi(argv[0] + 1)) < 0)
      error("Usage: sort -dfnr [+pos1] [-pos2]");
  }
  if (argc || pos1 > pos2)
    error("Usage: sort -dfnr [+pos1] [-pos2]");
}
