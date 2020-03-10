#include <stdio.h>

int main(void)
{
    int n_blank = 0, n_tab = 0, n_nl = 0;
    int c;

    while((c = getchar()) != EOF){
        if(c == ' ') n_blank++;  // count number of blanks
        if(c == '\t') n_tab++;  // count number of tabs
        if(c == '\n') n_nl++;  // count number of newlines
    }
    printf("the number of blanks is %d\nthe number of tabs is %d\nthe number of nwelines is %d\n", n_blank, n_tab, n_nl);
    return 0;
}