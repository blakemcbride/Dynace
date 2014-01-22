

/*  This file is used to help port the jumpto.s to a new platform  */

#include <stdio.h>

char	*obj = "Some object pointer";

char	*GenObj = "Some Generic Object Pointer";

typedef	int	(*ofun)();


int	Method(char *self, int a, int b, int c)
{
	printf("Method reached with args %s %d %d %d\n", self, a, b, c);
	return a + b + c;
}

ofun	FindMethod(char *obj, char *gen)
{
	return Method;
}

GenericFunction(char *self, ...)
{
	_jumpToMethod( FindMethod(self, GenObj) );
}

main(void)
{
	printf("Method is at %lx\n", (long unsigned int) &Method);
	/* both calls to Method should look alike to Method  */
	int	r = Method(obj, 1, 2, 3);
	printf("Value returned from GenericFunction = %d\n", r);
	r = GenericFunction(obj, 1, 2, 3);
	printf("Value returned from GenericFunction = %d\n", r);
	return 0;
}
