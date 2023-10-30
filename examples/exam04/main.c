
#include "generics.h"


int main(int argc, char *argv[])
{
	object	linkObject;


	InitDynace(&argc);


	/*  Create a new LinkObject */

	linkObject = gNew(LinkObject);


	/*  Print the entire link object out  */

	gPrint(linkObject, stdoutStream);


	/*  Add a new object to the beginning of the list and print  */

	gAddFirst(linkObject, gNewWithStr(String, "The first element added."));
	gPrint(linkObject, stdoutStream);


	/*  Add a new object to the beginning of the list and print */

	gAddFirst(linkObject, gNewWithDouble(DoubleFloat, 3.14159));
	gPrint(linkObject, stdoutStream);


	/*  Add a new object to the end of the list and print */

	gAddLast(linkObject, gNewWithLong(LongInteger, 186282L));
	gPrint(linkObject, stdoutStream);


	/*  Print the first element of the list and the whole list again  */

	gPrint(gFirst(linkObject), stdoutStream);
	gPrint(linkObject, stdoutStream);


	/*  Dispose of last element and print list  */

	gDeepDisposeLast(linkObject);
	gPrint(linkObject, stdoutStream);


	/*  Dispose of first element and print list  */

	gDeepDisposeFirst(linkObject);
	gPrint(linkObject, stdoutStream);

	

	/*  Dispose of the entire link object and all objects held  */
	/*  (again only necessary if garbage collector not used)    */

	gDeepDispose(linkObject);
	
	
	return 0;
}










