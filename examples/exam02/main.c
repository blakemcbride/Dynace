
#include "generics.h"


int main(int argc, char *argv[])
{
	/*  Declare a Dynace object  */

	object	myObj;


	InitDynace(&argc);


	/*  Create a new instance of class ShortInteger initialized to 6 */

	myObj = gNewWithInt(ShortInteger, 6);


	/*  Tell the object to print itself on stdout  */

	gPrint(myObj, stdoutStream);


	/*  Change the value contained in the object  */

	gChangeShortValue(myObj, 77);


	/*  Tell the object to print itself again  */

	gPrint(myObj, stdoutStream);


	/*  When an object is no longer needed it should be disposed of,
	    returning its memory to the heap.  (This would be done
	    automatically if the garbage collector was enabled.)  */

	gDispose(myObj);

	
	return 0;
}










