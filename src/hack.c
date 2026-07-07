#include <stdio.h>
#include <stdlib.h>

int hack_get_fb_(int *f, int *b)
{
	char *s;

        if (s = (char *)getenv("FOR"))
                *f = atoi(s);
        else
                *f = 0;
        if (s = (char *)getenv("BACK"))
                *b = atoi(s);
        else
                *b = 1;

        printf("for=%d, back=%d\n", *f, *b);
}
