
#include "generics.h"


int main(int argc, char *argv[])
{
	object	strDict, seq, strAss;


	InitDynace(&argc);


	/*  Create, initialize and print an instance of StringDictionary  */

	strDict = gNewWithInt(StringDictionary, 49);
	gAddStr(strDict, "Key 1", gNewWithStr(String, "The first value added."));
	gAddStr(strDict, "Other key", gNewWithDouble(DoubleFloat, 3.14159));
	gAddStr(strDict, "abcd", gNewWithLong(LongInteger, 186282L));
	gPrint(strDict, stdoutStream);


	/*  Create an instance of SetSequence (seq = gSequence()) and
	    sequence through the StringAssociations (strAss) in the
	    StringDictionary (strDict) printing the value associated
	    with each.  */

	for (seq = gSequence(strDict)  ;  strAss = gNext(seq) ; )
		gPrint(gValue(strAss), stdoutStream);



	gDeepDispose(strDict);
	
	return 0;
}










