#include <stdio.h>

// print Fahrenheit-Celsius table for fahr = 0, 20, ..., 3000; floating-point version

int main(void)
{
    float fahr, celsius;
    int lower, upper, step;

    lower = 0;
    upper = 300;
    step = 20;

    printf("Celsius | Fahrenheit\n\n");
    
    fahr = lower;
    while(fahr <= upper) {
        celsius = (5.0/9.0) * (fahr-32.0);
        printf("%6.1f %3.0f\n",celsius, fahr);
        fahr = fahr + step;
    }
    return 0;
}