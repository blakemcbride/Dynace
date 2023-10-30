
#include "generics.h"


int main(int argc, char *argv[])
{
	object	myObj, seq, lnk;


	InitDynace(&argc);


	myObj = gSubClasses(Object);
	for (seq=gSequence(myObj) ; lnk = gNext(seq) ; )
		gPrint(lnk, stdoutStream);
	gDispose(myObj);


	printf("\n\n");

	myObj = gSuperClasses(Dictionary);
	for (seq=gSequence(myObj) ; lnk = gNext(seq) ; )
		gPrint(lnk, stdoutStream);
/*	gDispose(myObj);  */


	printf("\n\n");

	printf("%s\n", gIsKindOf(myObj, Object) ? "Yes" : "No");
	printf("%s\n", gIsKindOf(myObj, LinkObject) ? "Yes" : "No");
	printf("%s\n", gIsKindOf(myObj, LinkList) ? "Yes" : "No");
	printf("%s\n", gIsKindOf(myObj, Dictionary) ? "Yes" : "No");


	printf("\n\n");

	printf("%s\n", IsaClass(Link) ? "Yes" : "No");
	printf("%s\n", IsaClass(myObj) ? "Yes" : "No");

	printf("\n\n");

	printf("%s\n", IsInstanceOf(myObj, LinkObject) ? "Yes" : "No");
	printf("%s\n", IsInstanceOf(myObj, LinkList) ? "Yes" : "No");

	printf("\n\n");

	printf("%s\n", RespondsTo(myObj, gFirst) ? "Yes" : "No");
	printf("%s\n", RespondsTo(myObj, gDispose) ? "Yes" : "No");
	printf("%s\n", RespondsTo(myObj, gShortValue) ? "Yes" : "No");


	return 0;
}










