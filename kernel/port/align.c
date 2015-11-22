

#include <stdio.h>

#define X(y)  printf(#y " = %lld (%d)\n", (long long)(&y.p2 - &y.p1), (int)sizeof(y.val))

struct	{
	void	*p1;
	char	val;
	void	*p2;
}	V1;


main()
{
	X(V1);
	return 0;
}
