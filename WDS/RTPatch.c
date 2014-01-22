
/*  Copyright 1995 Algorithms Corporation  */


#include "generics.h"
#include "scheme.h"
#include "patchwin.h"


typedef UINT 
    __declspec( dllimport ) 
    (CALLBACK *pRTPatchApply32)( LPSTR CmdLine, 
			    LPVOID (CALLBACK * CallBackFn)(UINT, LPVOID),
			    BOOL WaitFlag);

typedef UINT
    __declspec( dllimport )
    (__cdecl *pRTPatchApply32NoCall)( LPSTR CmdLine );

static	char	*scheme_patch_callback;

static	LPVOID	CALLBACK	patchCallback(UINT Id, LPVOID Parm)
{
	char	buf[1024];
	sprintf(buf, "(%s %d \"%s\")", scheme_patch_callback, (int) Id, Parm);
	gExecuteStringNR(Scheme, buf);
	return "";
}

SCHEME_FUNCTION(rtpatchapply32)
{
	static	char	fun[] = "RTPatchApply32";
	object	dll;
	pRTPatchApply32 RTPatchApply32;
	int	r;

	SCHEME_ARG_COUNT(3);
	SCHEME_CHK_ARG_STRING(0);
	SCHEME_CHK_ARG_STRING(1);
	SCHEME_CHK_ARG_INT(2);
	dll = gLoadLibrary(DynamicLibrary, "patchw32.dll");
	if (!dll)
		return scheme_make_integer(-1);
	RTPatchApply32 = (pRTPatchApply32) gGetProcAddress(dll, "RTPatchApply32");
	if (!RTPatchApply32) {
		gDispose(dll);
		return scheme_make_integer(-2);
	}
	scheme_patch_callback = SCHEME_STR_VAL(argv[1]);
	r = RTPatchApply32(SCHEME_STR_VAL(argv[0]), strlen(scheme_patch_callback)?patchCallback:NULL, SCHEME_INT_VAL(2));
	gDispose(dll);
	return scheme_make_integer(r);
}

SCHEME_FUNCTION(rtpatchapply32nocall)
{
	static	char	fun[] = "RTPatchApply32NoCall";
	object	dll;
	pRTPatchApply32NoCall RTPatchApply32NoCall;
	int	r;

	SCHEME_ARG_COUNT(1);
	SCHEME_CHK_ARG_STRING(0);
	dll = gLoadLibrary(DynamicLibrary, "patchw32.dll");
	if (!dll)
		return scheme_make_integer(-1);
	RTPatchApply32NoCall = (pRTPatchApply32NoCall) gGetProcAddress(dll, "RTPatchApply32NoCall");
	if (!RTPatchApply32NoCall) {
		gDispose(dll);
		return scheme_make_integer(-2);
	}
	r = RTPatchApply32NoCall(SCHEME_STR_VAL(argv[0]));
	gDispose(dll);
	return scheme_make_integer(r);
}

void	Scheme_init_RTPatch(void)
{
	ADD_FUN(RTPatchApply32, rtpatchapply32);
	ADD_FUN(RTPatchApply32NoCall, rtpatchapply32nocall);
}	

