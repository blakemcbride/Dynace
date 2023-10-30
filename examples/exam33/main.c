
#include "generics.h"

extern void MA_compact(void);

int main(int argc, char *argv[])
{
	object	a, b, c;


	InitDynace(&argc);

	/*  I must use the garbage collector because I am not manually
	    disposing of the objects I will be creating.  */
	
	gSetMemoryBufferArea(Dynace, 50000L);


	/*  Note that you can initialize the string with either a C string
	    or an instance of the String class.  */

	a = gNewWithStr(String, "string 1");
	b = gNewWithObj(String, a);
	c = gNewWithStr(String, "string 3");

	gPrint(a, stdoutStream);
	gPrint(b, stdoutStream);
	gPrint(c, stdoutStream);

	printf("\nChangeValue\n");
	gChangeStrValue(b, "string 2");
	gPrint(b, stdoutStream);
	gDispose(b);
	

	/*  This demonstrates how the memory compactor (MA_compact) may
	    move a string to compact memory usage.  This test works because
	    we deleted object b which was between objects a and c.  */
	
	printf("\nMA_compact\n");
	gPrint(a, stdoutStream);
	printf("c string = %ld\n", (long) gStringValue(c));
	gPrint(c, stdoutStream);
	MA_compact();
	gPrint(a, stdoutStream);
	printf("c string = %ld\n", (long) gStringValue(c));
	gPrint(c, stdoutStream);

	printf("\nSprintf\n");
	gPrint(vSprintf(String, "s=%s, f=%f", "Hello there", 3.141), stdoutStream);

	/*  Note that the argument list to vBuild MUST ALWAYS end with a
	    NULL!!  */
	printf("\nClass Build\n");
	gPrint(vBuild(String, "Hello there ", a, NULL), stdoutStream);

	printf("\nStringValue\n%s\n", gStringValue(a));

	printf("\nCompare\n");
	printf("%d\n", gCompare(a, (object) "string 1"));
	printf("%d\n", gCompare(a, gNewWithStr(String, "string 1")));
	printf("%d\n", gCompare(a, c));
	printf("%d\n", gCompare(c, a));
	printf("%d\n", gCompareI(a, (object) "stRing 1"));
	printf("%d\n", gCompareN(a, (object) "str", 3));

	printf("\nEqual\n");
	b = gNewWithStr(String, "A Test");
	printf("%d\n", gEqual(b, (object) "A Test"));
	printf("%d\n", gEqual(b, gNewWithStr(String, "A Test2")));
	printf("%d\n", gEqual(b, gNewWithStr(String, "A Test")));

	printf("\nLength = %d\n", gSize(a));

	printf("\nAppend\n");
	gAppend(a, (object) " Appended");
	gAppend(a, gNewWithStr(String, " Twice"));
	gPrint(a, stdoutStream);

	/*  Note that the argument list to vBuild MUST ALWAYS end with a
	    NULL!!  */
	printf("\nBuild\n");
	vBuild(a, c, "Part 2", gNewWithStr(String, " Part 3"), NULL);
	vBuild(a, NULL, " The last part", NULL);
	gPrint(a, stdoutStream);

	printf("\nCharacter 8 = %c\n", gCharValueAt(a, 8));
	gChangeCharAt(a, 8, 'X');
	printf("Character 8 = %c\n", gCharValueAt(a, 8));

	printf("\nCase conversion\n");
	gToLower(a);
	gPrint(a, stdoutStream);
	gToUpper(a);
	gPrint(a, stdoutStream);

	printf("\nSubString\n");
	gPrint(gSubString(a, 8, 4), stdoutStream);

	printf("\nTake/Drop\n");
	gTake(a, 8);
	gPrint(a, stdoutStream);
	gDrop(a, -1);
	gPrint(a, stdoutStream);
	gTake(a, -20);
	gPrint(a, stdoutStream);

	/*  Since the following methods modify the string object I always
	    use a copy so I don't have to keep recreating the original.
	    Note that I am depending on the garbage collector to get rid
	    of the unneeded intermediate objects.  */

	printf("\nStrip/Justify\n");
	a = gNewWithStr(String, "STRING");
	gTake(a, -16);
	gTake(a, 55);
	gPrint(a, stdoutStream);
	gPrint(gStripLeft(gCopy(a)), stdoutStream);
	gPrint(gStripRight(gCopy(a)), stdoutStream);
	gPrint(gStripCenter(gCopy(a)), stdoutStream);
	gPrint(gJustifyLeft(gCopy(a)), stdoutStream);
	gPrint(gJustifyRight(gCopy(a)), stdoutStream);
	gPrint(gJustifyCenter(gCopy(a)), stdoutStream);


#ifdef	EXTRA
	printf("\nProcess\n");
	a = gNewWithStr(String, "\\tHello\\tThere\\nNext line\\a");
	printf("\nPrint Length = %d\n", gPrintLength(a));
	gPrint(a, stdoutStream);
	gProcess(a);
	gPrint(a, stdoutStream);
#endif
	
	return 0;
}










