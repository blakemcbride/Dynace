
#include "generics.h"


int main(int argc, char *argv[])
{
	object	obj1, obj2;

	InitDynace(&argc);

	/*  Create two instances of the Class1 class.  */

	obj1 = gNew(Class1);
	obj2 = gNew(Class1);

	/*  Set each instance to a different name.  */

	gSetName(obj1, "Object One");
	gSetName(obj2, "Object Two");

	/*  Display the independent values associated with each instance.  */

	printf("obj1's name is %s\n", gGetName(obj1));
	printf("obj2's name is %s\n", gGetName(obj2));


	gDispose(obj1);
	gDispose(obj2);

	return 0;
}










