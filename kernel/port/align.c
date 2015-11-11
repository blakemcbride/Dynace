

#include <stdio.h>

#define X(y)  printf(#y " = %lld (%d)\n", (long long) &y, (int) sizeof(y))

typedef	struct	{
	void	*cls;
	unsigned short  tag;
}	Type;

typedef	struct	{
	void	*a;
	unsigned short  b;
	void	*c;
}	Type2;


main()
{
	Type	v1,  v2;
	char	c, d;
	Type	v3;
	Type2	v4;

	X(v1);
	X(v2);
	X(c);
	X(d);
	X(v3);
	X(v4);
	X(v4.a);
	X(v4.b);
	X(v4.c);
	return 0;
}
