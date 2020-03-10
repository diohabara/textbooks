#include <stdio.h>

static char daytab[2][13] = {
    { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
    { 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

int main(int argc, char const* argv[])
{
    int day_of_year(int, int, int);
    void month_day(int, int, int*, int*);
    int year = 2019, month = 2, day = 28;
    int yearday;
    printf("%d/%d/%d is %dth day\n", year, month, day, yearday = day_of_year(year, month, day));

    int pmonth, pday;
    month_day(year, yearday, &pmonth, &pday);
    printf("%dth day is %d/%d/%d\n", yearday, year, pmonth, pday);
    return 0;
}

int day_of_year(int year, int month, int day)
{
    int i, leap;

    leap = year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
    if ((month < 1 || 12 < month) || (day < 1 || *(*(daytab + leap) + month) < day))
        return -1;
    for (i = 1; i < month; i++)
        day += *(*(daytab + leap) + i);
    return day;
}

void month_day(int year, int yearday, int* pmonth, int* pday)
{
    int i, leap;

    leap = year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
    for (i = 1; yearday > *(*(daytab + leap) + i); i++)
        yearday -= *(*(daytab + leap) + i);
    if (i > 12 && yearday > *(*(daytab + leap) + 12)) {
        *pmonth = -1;
        *pday = -1;
    } else {
        *pmonth = i;
        *pday = yearday;
    }
}
