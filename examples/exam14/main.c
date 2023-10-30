
#include "generics.h"


int main(int argc, char *argv[])
{
	object	obj1, obj2;

	InitDynace(&argc);

	/*  Create two instances of the Class1 class.  This will evoke the
	    newly defined New class method through the gNew generic.  */

	obj1 = gNew(Class1);
	obj2 = gNew(Class1);

	/*  Set each instance to a different name.  */

	gSetName(obj1, "Object One");
	gSetName(obj2, "Object Two");

	/*  Display the independent values associated with each instance.  */

	printf("obj1's name is %s\n", gGetName(obj1));
	printf("obj2's name is %s\n", gGetName(obj2));

	/*  Evoke the NumInstances class method through the gNumInstances
	    generic.  Notice that the argument passed is the class - not
	    an instance object.  This is because class methods are associated
	    with classes.  The returned value is printed.  */
	
	printf("Number of instances = %d\n", gNumInstances(Class1));

	/*  Dispose one of the instances.  */
	gDispose(obj1);

	/*  Display the count again.  */
	
	printf("Number of instances = %d\n", gNumInstances(Class1));

	/*  Dispose and display the count again.  */

	gDispose(obj2);
	printf("Number of instances = %d\n", gNumInstances(Class1));

	return 0;
}










