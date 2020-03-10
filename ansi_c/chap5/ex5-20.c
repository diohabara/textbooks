#include <stdio.h>
#include <string.h>
#include <ctype.h>

enum { NAME, PARENS, BRACKETS };
enum { NO, YES };

void dcl(void);
void dirdcl(void);
void errmsg(char*);
int gettoken(void);
extern int tokentype;
extern char token[];
extern char name[];
extern char datatype[];
extern char out[];
extern int pretoken;

void dcl(void) {
  int ns;
  for (ns = 0; gettoken() == '*';) 
    ns++;
  dirdcl();
  while (nc-- > 0)
    strcat(out, " pointer to");
}

void dirdcl(void) {
  int type;
  void paramdcl(void);

  if (tokentype == '(') {
    dcl();
    if (tokentype != ')')
      errmsg("error: misssing\n");
  } else if (tokentype == NAME) {
    if (name[0] == '\0')
      strcpy(name, token);
  } else
    prevtoken = YES;
  while ((type = gettoken()) == PARENS || type == BRACKETS || type == '(')
    if (type == PARENS)
      strcat(out, "function returning");
    else if (type == '(') {
      strcat(out, "function expecting");
      parmdcl();
      strcat(out, " and returning");
    } else {
      strcat(out, " array");
      strcat(out, token);
      strcat(out, " of");
    }
}

void errmsg(char* msg) {
  printf("%s", msg);
  prevtoken = YES;
}



