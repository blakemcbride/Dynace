
#include "generics.h"


int main(int argc, char *argv[])
{
	object	linkObject, seq, obj;


	InitDynace(&argc);


	/*  Create and initialize a LinkObject  */

	linkObject = gNew(LinkObject);
	gAddFirst(linkObject, gNewWithStr(String, "The first element added."));
	gAddFirst(linkObject, gNewWithDouble(DoubleFloat, 3.14159));
	gAddLast(linkObject, gNewWithLong(LongInteger, 186282L));
	gPrint(linkObject, stdoutStream);


	/*  Create an instance of LinkObjectSequence (with gSequence)
	    and use it to sequence through all the elements of the
	    list.  Print each element.  */

	for (seq = gSequence(linkObject)  ;  obj = gNext(seq) ; )
		gPrint(obj, stdoutStream);


	/*  Print entire list to show its not changed.  */

	gPrint(linkObject, stdoutStream);


	gDeepDispose(linkObject);
	
	return 0;
}










