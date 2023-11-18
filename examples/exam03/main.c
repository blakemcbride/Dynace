
#include "generics.h"


int main(int argc, char *argv[])
{
	object	myObj;

	InitDynace(&argc);

	/*  Create and use an instance of the ShortInteger class */

	myObj = gNewWithInt(ShortInteger,  6);
	gPrint(myObj, stdoutStream);
	gChangeShortValue(myObj, 77);
	gPrint(myObj, stdoutStream);
	gDispose(myObj);

	/*  Create and use an instance of the String class */

	myObj = gNewWithStr(String, "Hello World.");
	gPrint(myObj, stdoutStream);
	gChangeStrValue(myObj, "New String Value");
	gPrint(myObj, stdoutStream);
	gDispose(myObj);



	/*  Create and use an instance of the DoubleFloat class */

	myObj = gNewWithDouble(DoubleFloat, 3.1415926);
	gPrint(myObj, stdoutStream);
	gChangeDoubleValue(myObj, 7.2144);
	gPrint(myObj, stdoutStream);
	gDispose(myObj);


	/*  Create and use an instance of the Character class */

	myObj = gNewWithChar(Character, 'A');
	gPrint(myObj, stdoutStream);
	gChangeCharValue(myObj, 'B');
	gPrint(myObj, stdoutStream);
	gDispose(myObj);


	/*  Create and use an instance of the Date class */

	myObj = gNewWithLong(Date, 19930802L);
	gPrint(myObj, stdoutStream);
	gChangeLongValue(myObj, 19580608L);
	gPrint(myObj, stdoutStream);
	gDispose(myObj);

	return 0;
}










