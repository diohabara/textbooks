#include <stdio.h>
#include <time.h>
#define MAXLINE 1000000

int binsearch1(int x, int v[], int n);
int binsearch2(int x, int v[], int n);

int main(int argc, char const* argv[])
{
    int v[MAXLINE];
    int target = 17;
    clock_t start, end;

    // the order of two binary search is the same, O(n^2)
    // the difference of time complexity is small, but the later one is slightly faster (in expense of readability)
    start = clock();
    binsearch1(target, v, MAXLINE);
    end = clock();
    printf("binsearch1 takes %f\n", (double)(end - start)/(CLOCKS_PER_SEC));

    start = clock();
    binsearch2(target, v, MAXLINE);
    end = clock();
    printf("binsearch2 takes %f\n", (double)(end - start)/(CLOCKS_PER_SEC));

    return 0;
}

int binsearch1(int x, int v[], int n)
{
    int low, high, mid;

    low = 0;
    high = n - 1;
    while (low <= high) {
        mid = (low + high) / 2;
        if (x < v[mid])
            high = mid - 1;
        else if (x > v[mid])
            low = mid + 1;
        else
            return mid;
    }
    return -1;
}

int binsearch2(int x, int v[], int n)
{
    int low, high, mid;

    low = 0;
    high = n - 1;
    mid = (low + high) / 2;
    while (low <= high && x != v[mid]) {
        if (x < v[mid])
            high = mid - 1;
        else
            low = mid + 1;
        mid = (low + high) / 2;
    }
    if (x == v[mid])
        return mid;
    else
        return -1;
}
