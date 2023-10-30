
#include "generics.h"


int main(int argc, char *argv[])
{
	object	a, b, c;

	InitDynace(&argc);

	/*  I must use the garbage collector because I am not manually
	    disposing of the objects I will be creating.  */
	
	gSetMemoryBufferArea(Dynace, 50000L);


	a = gNewWithInt(ShortInteger, 7);
	b = gNewWithDouble(DoubleFloat, 3.131);

	gPrint(a, stdoutStream);
	gPrint(b, stdoutStream);

	c = gFormatNumber(a, "", 0, 2);
	gPrint(c, stdoutStream);

	c = gFormatNumber(b, "", 0, 2);
	gPrint(c, stdoutStream);
	
	b = gNewWithDouble(DoubleFloat, 2345678.87);
	c = gFormatNumber(b, "CD", 0, 2);
	gPrint(c, stdoutStream);

	a = gToday(Date);
	c = gFormatDate(a, "%N/%D/%y");
	gPrint(c, stdoutStream);
	c = gFormatDate(a, "%N/%D/%Y");
	gPrint(c, stdoutStream);
	c = gFormatDate(a, "%W  %M %d%s, %Y  %T");
	gPrint(c, stdoutStream);
	c = gFormatDate(a, "%m. %d, %Y");
	gPrint(c, stdoutStream);
	c = gFormatDate(a, "%W the %d%s of %M");
	gPrint(c, stdoutStream);

	return 0;
}










