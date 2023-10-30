
#include "generics.h"


int main(int argc, char *argv[])
{
	object	x;

	InitDynace(&argc);

	x = gIota(ShortArray, 40);
/*	x = vNew(ShortArray, 2, 5, 8);		*/
	vReshape(x, 2, 5, 8);
	vChangeShortValue(x, 88, 3, 4);

	gPrint(x, stdoutStream);

	gDispose(x);

	return 0;
}










