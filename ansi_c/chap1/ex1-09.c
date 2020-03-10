#include <stdio.h>

int main(void)
{
    int c, is_blank = 0;
    int number_blank;
    while((c = getchar()) != EOF){
        if(c == ' ' && is_blank == 0){
            is_blank = 1;
            putchar(c);
        }
        else if(c == ' ' && is_blank != 0){
            ; // do nothing
        }
        else{
            is_blank = 0;
            putchar(c);
        }
        }
    return 0;
}