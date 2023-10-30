
#include "generics.h"

/*  you need to include the file which declares alloca()  */
#include <malloc.h>

void	fun(int);


int main(int argc, char *argv[])
{
	int	m=1, i;
	
	InitDynace(&argc);

	if (argc > 1)
		m = atoi(argv[1]);

	for (i=0 ; i++ < m ; )
		fun(i);
	
	return 0;
}

void	fun(int i)
{
	object	x;

#if 1
	x = StackAlloc(ShortInteger);      /*  stack allocation   */
#else
	x = gAlloc(ShortInteger);          /*  heap allocation  */
#endif
	gChangeShortValue(x, i);
	gPrint(x, stdoutStream);
}










