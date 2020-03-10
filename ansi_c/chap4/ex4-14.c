#include <stdio.h>
#define swap(t, x, y) \
    {                 \
        t _z;         \
        _z = y;       \
        y = x;        \
        x = _z;       \
    }

int main(int argc, char const* argv[])
{
    int x = 1, y = 2;
    swap(int, x, y);
    printf("x=%d\ty=%d\n", x, y);
    return 0;
}
