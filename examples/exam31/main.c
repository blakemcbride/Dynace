
#include "generics.h"


static	object	(*old_gPrint)(object, object);


static  object	new_gPrint(object self, object stream)
{
	printf("\n\nxxx\n");
	old_gPrint(self, stream);
	printf("xxxx\n\n");
	return self;
}

int main(int argc, char *argv[])
{
	object	myObj;


	InitDynace(&argc);


	/*  Create an instance of the ShortInteger class */

	myObj = gNewWithInt(ShortInteger,  6);

	/*  print it out using the normal gPrint generic  */

	gPrint(myObj, stdoutStream);

	/*  save the old generic function in the variable old_gPrint  */

	old_gPrint = gPrint;

	/*  make gPrint execute new_gPrint  */

	gPrint = new_gPrint;

	/*  execute the new gPrint generic  */
	
	gPrint(myObj, stdoutStream);


	/*  return gPrint to its original generic  */

	gPrint = old_gPrint;

	/*  try it out  */

	gPrint(myObj, stdoutStream);

	gDispose(myObj);


	return 0;
}










