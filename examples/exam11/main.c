
#include "generics.h"


int main(int argc, char *argv[])
{
	/*  declare a variable to hold the object  */
	object	myObj;

	InitDynace(&argc);

	/*  Create an instance of the new class.  Note that the gNew generic
	    is actually running the New method associated with the Object
	    class since Class1 doesn't explicitly define this method.  */
	    
	myObj = gNew(Class1);

	/*  If the garbage collector is not being used you must dispose of
	    an object which is no longer needed (unless of course your program
	    is terminating, in which case all the objects will be freed
	    anyway.)  This method is also being inherited from the Object
	    class.  */

	gDispose(myObj);

	return 0;
}










