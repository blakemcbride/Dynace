
#include "generics.h"



int main(int argc, char *argv[])
{
	object	myObj;
	gPrint_t	print_method;
	int	i;

	InitDynace(&argc);


	/*  Create an instance of the ShortInteger class */

	myObj = gNewWithInt(ShortInteger, 6);

	/*  perform the runtime overhead of finding the method once  */

	print_method = imiPointer(myObj, gPrint);

	for (i=0 ; i++ != 10 ; )

		/*  use it over and over without any lookup overhead  */
		/*  Note that it's the same as:  gPrint(myObj, stdoutStream);*/

		print_method(myObj, stdoutStream);


	gDispose(myObj);

	return 0;
}










