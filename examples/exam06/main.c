
#include "generics.h"


int main(int argc, char *argv[])
{
	object	strDict;


	InitDynace(&argc);


	/*  Create a new StringDictionary to hold roughly 49 elements.
	    Note that although the dictionary is set up to hold roughly 49
	    elements, it may actually hold any number,  it's just that
	    the effeciency of the dictionary will start to go down at
	    around 49 elements.  */

	strDict = gNewWithInt(StringDictionary, 49);


	/*  Print the entire link object out  */

	gPrint(strDict, stdoutStream);


	/*  Add a new object to the dictionary and print  */

	gAddStr(strDict, "Key 1", gNewWithStr(String, "The first value added."));
	gPrint(strDict, stdoutStream);


	/*  Add a new object to the dictionary and print */

	gAddStr(strDict, "Other key", gNewWithDouble(DoubleFloat, 3.14159));
	gPrint(strDict, stdoutStream);

	/*  Add a new object to dictionary and print */

	gAddStr(strDict, "abcd", gNewWithLong(LongInteger, 186282L));
	gPrint(strDict, stdoutStream);


	/*  Find and print the value associated with "Other key" */

	gPrint(gFindValueStr(strDict, "Other key"), stdoutStream);


	/*  Dispose of one of the key/value pairs and print dictionary */

	gDeepDisposeStr(strDict, "Key 1");
	gPrint(strDict, stdoutStream);


	/*  Dispose of the entire link object and all objects held  */
	/*  (again only necessary if garbage collector not used)    */

	gDeepDispose(strDict);
	
	
	return 0;
}










