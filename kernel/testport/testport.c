
struct large_s {
    double r, g, b;
    char a[8];
    double c, m, y;
};

struct small_s {
    int i;
};

#include <dynace.h>
#include <stdio.h>
#include <string.h>

#define PRINT_LARGE_S(s)			\
printf(#s					\
" = {\n"					\
"    r: %lf, g: %lf, b:%lf;\n"			\
"    a: \"%s\";\n"				\
"    c: %lf, m: %lf, y: %lf;\n};\n",		\
s.r, s.g, s.b, s.a, s.c, s.m, s.y)

#define PRINT_SMALL_S(s)			\
printf(#s					\
" = {\n"					\
"    i: %i;\n};\n",				\
s.i)

static int
Method_1(char *self, int i, double d, struct large_s S, char c,
	 struct small_s s)
{
    printf("\n--------- in Method_1()\n");
    printf("self: %s\n", self);
    printf("d: %lf\n", d);
    PRINT_LARGE_S(S);
    printf("c: %c\n", c);
    PRINT_SMALL_S(s);
    return 1;
}

static double
Method_2(char *self, struct large_s S)
{
    printf("\n--------- in Method_2()\n");
    printf("self: %s\n", self);
    PRINT_LARGE_S(S);
    return S.r;
}

static struct large_s
Method_3(char *self, struct large_s S)
{
    printf("\n--------- in Method_3()\n");
    printf("self: %s\n", self);
    S.g /= 2.0;
    S.b /= 3.0;
    strcpy(S.a, "There!");
    PRINT_LARGE_S(S);
    return S;
}

static struct small_s
Method_4(char *self, struct small_s s)
{
    printf("\n--------- in Method_4()\n");
    printf("self: %s\n", self);
    s.i *= 3;
    PRINT_SMALL_S(s);
    return s;
}

ofun
_FindMethod(object o1, object o2)
{
    static int invocation = 0;

    switch(++invocation) {
    case 1: return (ofun)Method_1;
    case 2: return (ofun)Method_2;
    case 3: return (ofun)Method_3;
    case 4: return (ofun)Method_4;
    default: return NULL;
    }
}

static object obj = (object)"Some object pointer";

int
main()
{
    struct large_s S;
    struct small_s s;
    int i_result;
    double d_result;
    struct large_s S_result;
    struct small_s s_result;

    printf("\n--------- in main()\n");
    S.r = 33333.33333; S.g = S.r * 2.0; S.b = S.r * 3.0;
    strcpy(S.a, "Hello!");
    S.c = 13579.013579; S.m = 24680.246802; S.y = 0.0;
    PRINT_LARGE_S(S);

    s.i = 13;
    PRINT_SMALL_S(s);

    i_result = gFunction_1(obj, 1, 2.0, S, '!', s);
    printf("i_result: %i\n", i_result);

    d_result = gFunction_2(obj, S);
    printf("d_result: %lf\n", d_result);

    S_result = gFunction_3(obj, S);
    PRINT_LARGE_S(S_result);
    PRINT_LARGE_S(S);

    s_result = gFunction_4(obj, s);
    PRINT_SMALL_S(s_result);
    PRINT_SMALL_S(s);
}

/* ---EOF--- testport.c */
