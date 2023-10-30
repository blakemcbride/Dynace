
#include "generics.h"


int main(int argc, char *argv[])
{
	object	obj;

	InitDynace(&argc);


	/*  Create and print an instance of short integer  */

	obj = gNewWithInt(ShortInteger, 8);
	gPrint(obj, stdoutStream);


	/*  Cause an error by trying to execute a generic which is not
	    associated with the ShortInteger class  (comment out the
	    next line after running the program to allow the following
	    errors to be demonstrated).  */

	gFirst(obj);


	/*  Create another error condition where there is a missing second
	    argument (uncomment out the next line to demo the error -
	    re-comment to see the following error) */

/*	gPrint(obj);    */



	/*  Cause an error by attempting to use an object after it has
	    been disposed.  */

	gDispose(obj);
	gPrint(obj, stdoutStream);

	

	return 0;
}










