

#include <stdio.h>

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
	char	c;
	Type	v3;
	Type2	v4;

	printf("v1 = %x\n", &v1);
	printf("v2 = %x\n", &v2);
	printf("v3 = %x\n", &v3,  c);
	printf("v4 = %x\n", &v4);
	printf("a = %x\n", &v4.a);
	printf("b = %x\n", &v4.b);
	printf("c = %x\n", &v4.c);
	return 0;
}
