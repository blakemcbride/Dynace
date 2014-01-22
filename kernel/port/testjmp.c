

#include <stdio.h>
#include <setjmp.h>

void	fun(jmp_buf jb);

main()
{
	register int a;
	jmp_buf	jb;

	a = 88;
	if (setjmp(jb))  {
		printf("main: 2 - a = %d\n", a);
		return 0;
	}
	printf("main: 1 - a = %d\n", a);
	fun(jb);
	return 0;
}

void	fun(jmp_buf jb)
{
	register int r1=1, r2=2, r3=3, r4=4, r5=5, r6=6, r7=7, r8=8;

	longjmp(jb, 1);
}



