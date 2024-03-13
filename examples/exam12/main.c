
#include "generics.h"


int main(int argc, char *argv[])
{
	object	myObj;
	char	*p;

	InitDynace(&argc);

	myObj = gNew(Class1);

	/*  Call the SetName method via the gSetName generic.  Notice that
	    the instance being affected is the first argument to the generic.
	    This is always the case.  */
	    
	gSetName(myObj, "Tom Jones");

	/*  Get a pointer to the name associated with myObj  */
	p = gGetName(myObj);
	
	/*  Display it.  */
	printf("The name is %s\n", p);


	gDispose(myObj);

	return 0;
}










