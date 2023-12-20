
#include "generics.h"


int main(int argc, char *argv[])
{
	object	obj;
	long	n;

	InitDynace(&argc);


	/*  The following line is used to turn on the automatic garbage
	    collector. Try this program with the following line commented
	    and uncommented to see the difference the garbage collector makes. */
	//gSetMemoryBufferArea(Dynace, 50000L);   



	/*  Create enough new objects to fill up all the memory of a PC
	    (since no objects are ever explictly disposed).  */

	for (n=0 ; n++ != 1250000L ; )  {
		if (!(n % 1000L))
			printf("%ld\n", n);
		obj = gNewWithDouble(DoubleFloat, 3.141);
	}

	printf("\nCurrent memory usage = %ld\n", gCurMemUsed(Dynace));
	
	return 0;
}










