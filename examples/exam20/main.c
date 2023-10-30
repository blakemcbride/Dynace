
#include "generics.h"


static	int	myThread(char *arg);


int main(int argc, char *argv[])
{
	object	t2;

	InitDynace(&argc);

	/*  Start the threader.  This also makes the currently running
	    main() function the first thread.  */

	StartThreader(argc);


	t2 = gNewThread(Thread, "t2", myThread, DEFAULT_PRIORITY, "t2", 1, 0);

	myThread("main");

	gWaitFor(t2);

	return 0;
}


static	int	myThread(char *arg)
{
	long	 i;
	object	 obj;

	obj = gNewWithLong(LongInteger, 0L);
	for (i=0L ; i++ != 100000L ; )  {
		gChangeLongValue(obj, i);
		gPrintValue(obj, stdoutStream);
		printf(" - %s\n", arg);
	}
	gDispose(obj);
	return 0;
}









