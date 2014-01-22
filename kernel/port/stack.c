
#include <stdio.h>

main(void)
{
	char	x[100];

	fun(x);
}

fun(char *x)
{
	if (x < (char *) &x)
		printf("STACK GROWS UP\n");
	else
		printf("STACK GROWS DOWN\n");
	if (&x[0] < &x[10])
		printf("ARRAYS GROW UP\n");
	else
		printf("ARRAYS GROW DOWN\n");
}
