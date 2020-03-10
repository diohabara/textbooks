#include <stdio.h>

int main(void)
{
    int c;
    while((c = getchar()) != EOF){
        if(c == '\t')
            printf("\\t");
        else if(c == '\b')
            printf("\\b");
        else if(c == '\\')
            printf("\\\\");
        else
            putchar(c);
        /* second version
        swtich(c){
            case '\t':
                printf("\\t");
                break;
            and so on...
        */
    }
    return 0;
}