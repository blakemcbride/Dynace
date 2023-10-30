
#include "generics.h"


static	int	cmpfun(object a, object b)
{
	int	av = gShortValue(a);
	int	bv = gShortValue(b);
	if (av < bv)
		return -1;
	else if (av == bv)
		return 0;
	else
		return 1;
}

#define	ADD(n)	gAddValue(t, gNewWithInt(ShortInteger, n), gNewWithStr(String, #n));

void find(object bt, int n)
{
	object	t = gNewWithInt(ShortInteger, n);
	object	f = gFind(bt, t);
	gDispose(t);
	if (f)
		printf("found %d as \"%s\"\n", n, gStringValue(f));
	else
		printf("%d not found\n", n);
}

void ge(object bt, int n)
{
	object	t = gNewWithInt(ShortInteger, n), t2;
	object	f = gFindGE(bt, t, &t2);
	gDispose(t);
	if (f)
		printf("found %d as %d, \"%s\"\n", n, (int) gShortValue(t2), gStringValue(f));
	else
		printf("%d not found\n", n);
}

void gt(object bt, int n)
{
	object	t = gNewWithInt(ShortInteger, n), t2;
	object	f = gFindGT(bt, t, &t2);
	gDispose(t);
	if (f)
		printf("found %d as %d, \"%s\"\n", n, (int) gShortValue(t2), gStringValue(f));
	else
		printf("%d not found\n", n);
}

void le(object bt, int n)
{
	object	t = gNewWithInt(ShortInteger, n), t2;
	object	f = gFindLE(bt, t, &t2);
	gDispose(t);
	if (f)
		printf("found %d as %d, \"%s\"\n", n, (int) gShortValue(t2), gStringValue(f));
	else
		printf("%d not found\n", n);
}

void lt(object bt, int n)
{
	object	t = gNewWithInt(ShortInteger, n), t2;
	object	f = gFindLT(bt, t, &t2);
	gDispose(t);
	if (f)
		printf("found %d as %d, \"%s\"\n", n, (int) gShortValue(t2), gStringValue(f));
	else
		printf("%d not found\n", n);
}

void first(object bt)
{
	object	key;
	object	f = gFindFirst(bt, &key);
	if (f)
		printf("found %d, \"%s\"\n", (int) gShortValue(key), gStringValue(f));
	else
		printf("not found\n");
}

void last(object bt)
{
	object	key;
	object	f = gFindLast(bt, &key);
	if (f)
		printf("found %d, \"%s\"\n", (int) gShortValue(key), gStringValue(f));
	else
		printf("not found\n");
}

void dispose(object bt, int n)
{
	object	t = gNewWithInt(ShortInteger, n);
	object	f = gDeepDisposeObj(bt, t);
	gDispose(t);
	if (f)
		printf("disposed %d\n", n);
	else
		printf("didn't dispose %d\n", n);
}


int main(int argc, char *argv[])
{
	object	t, k, d;

	InitDynace(&argc);

	t = gNew(BTree);
	gSetFunction(t, cmpfun);

	ADD(10);
	ADD(20);
	ADD(30);
	ADD(40);
	ADD(50);
	ADD(60);
	ADD(70);
	ADD(80);
	ADD(90);
	ADD(100);

	for (k=NULL ; d = gFindPrev(t, &k) ; )
		printf("found %d, \"%s\"\n", (int) gShortValue(k), gStringValue(d));

#if 0

//	gPrint(t, stdoutStream);

//	first(t);
//	last(t);

//	find(t, 39);

	gPrint(t, stdoutStream);
#endif

	return 0;
}










