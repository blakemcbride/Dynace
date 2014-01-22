
struct large_s {
    double r, g, b;
    char a[8];
    double c, m, y;
};

struct small_s {
    int i;
};

#include <dynace.h>

defGeneric( int,		gFunction_1)
defGeneric( double,		gFunction_2)
defGeneric( struct large_s,	gFunction_3)
defGeneric( struct small_s,	gFunction_4)

