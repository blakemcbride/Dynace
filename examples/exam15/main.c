
#include "generics.h"


int main(int argc, char *argv[])
{
	object	obj1, obj2;

	InitDynace(&argc);

	/*  Create two instances of the Class1 class.  Note the new arguments.
	    This is done to reflect the argument change performed on the
	    New method associated with the Class1 class.  See class1.d  */

	obj1 = gNewWithInt(Class1, 45);
	obj2 = gNewWithInt(Class1, 36);

	/*  In order to insure the effectiveness of our new object's
	    initialization, print the code associated with each instance.  */
	    
	printf("obj1's code = %d\n", gGetCode(obj1));
	printf("obj2's code = %d\n", gGetCode(obj2));

	gDispose(obj1);
	gDispose(obj2);

	return 0;
}










