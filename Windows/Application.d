/*
  Copyright (c) 1996 Blake McBride
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/




//#include "logfile.h"

defclass  Application  {
 class:
	HINSTANCE	cInstance;
	HINSTANCE	cPrevInstance;
	LPSTR		cCmdLine;
	int		cCmdShow;
	cNAME;				/*  application name  */

	int		(*cTrace)();	/*  original trace function  */

	HANDLE		cMutex;		/*  to avoid multiple instances	*/

	cGlobals;			/*  StringDictionary of application
					    global objects  */

	cAtExit;			/*  things to dipose of at exit  */
	cAtFatalExit;			/*  things to dipose of at a fatal exit  */

	/*  app wide default resources  */

	cCursor;
	cIcon;
	cFont;
	cTextBrush;
	cBackBrush;	/*  background brush  */

	int	cScalingMode;
	int	cSysCharHeight;		/*  used for scaling  */
	int	cSysCharWidth;

	int	cVSize;			/*  screen total size  */
	int	cHSize;

	/*  vars dealing with F1 hook  */

	HHOOK	cHook;			/*  handle to hook function	*/
	HOOKPROC	cHI;		/*  hook function instance	*/

	cCtl3d;				/*  Ctl3d dynamic library	*/
	int	cNoCtl3d;		/*  1=Don't use ctl3d		*/

	/*  misc.   */

	int	cUsingCOM;

	char	*cBuf;			/*  temporary buffer used throughout the system  */

	int	cReturnAsTab;		/*  Make return key act like tab on dialogs  */

	int	cNumArgs;		/*  Holds the number of command line args	*/
	cArgPointers;			/*  Holds pointers to the command line args	*/
	int	cAutoDisposeAtExit;	/*  Flag determining whether to auto dispose object at exit */
	int	cExiting;		/*  1= in the process of exiting  */

	int	cMessageMode;		/*  0=suppress gMessage, 1=gMessage as normal */
	cMessageObjects;		/*  Linked list of objects for messages */

	char	*cErrorMessage;

	char	cAltColor;  //	using alternate color
	int	cAltRed;
	int	cAltGreen;
	int	cAltBlue;

	int	cLanguage;	//	current language 0=English, 1=Spanish
	
 init:	class_init;
};


#include <ctype.h>
#include <string.h>
#include "demo.h"


#ifdef	WIN32
#include <commctrl.h>
#endif

#define	BUFSIZE		4000

static char * languages[]={"English","Spanish"};  //English needs to stay in position 0



/*  blank lines necessary because of a bug in Microsoft C 32 2.2  */




static	void	AppError();
static	int	Trace();


static	HINSTANCE	GhInstance;
static	HINSTANCE	GhPrevInstance;
static	LPSTR		GlpszCmdLine;
static	int		GcmdShow;

DEMO_VARS;

static	LRESULT	CALLBACK	KeyboardProc(int nCode, WPARAM wParam, LPARAM lParam);
//static	LRESULT	CALLBACK	MessageProc(int nCode, WPARAM wParam, LPARAM lParam);

static	void	free_objects(void);
static	void	setup_Ctl3d(void);
static	void	terminate_Ctl3d(void);
static	void	fix_disable_font_color(void);
static	void	disposeAtExit(object ae);


extern	void	SetWDSOptions(void);
extern	void	_link_WDS_main(void);

int	_WDS_Application = 0;  //  add support for console Win32 apps


void	_init_WDS(HINSTANCE hInstance, 
		  HINSTANCE hPrevInstance, 
		  LPSTR	    lpszCmdLine, 
		  int	    cmdShow,
		  void	    *ss)
{
	object	meth;
	int	r;


#ifndef	_WIN32
	for (r=96 ; !SetMessageQueue(r) ; r -= 8);
#endif
	_WDS_Application = 1;

	DEMO_INIT;

	GhInstance     = hInstance;
	GhPrevInstance = hPrevInstance;
	GlpszCmdLine   = lpszCmdLine;
	GcmdShow       = cmdShow;

	_link_WDS_main();  // causes my main to link

	InitDynace(ss);

	Application;

	cBuf = malloc(BUFSIZE);

	/*  reroute gError output  */
	gChangeFunction(gFindMethodObject(Object, Generic(gError), 1), (ofun) AppError);

	/*  reroute gTrace::Dynace  */
	meth = gFindMethodObject(ClassOf(Dynace), Generic(gTrace), 1);
	cTrace = (int (*)()) gFunction(meth);
	gChangeFunction(meth, (ofun) Trace);

	SetWDSOptions();   //  may be application specific

	cCursor    = gLoadSys(SystemCursor, IDC_ARROW);
	cIcon      = gLoadSys(SystemIcon, IDI_APPLICATION);
	cFont      = mLoad(SystemFont, SYSTEM_FONT);
	cTextBrush = vNew(SystemBrush, COLOR_WINDOWTEXT);
	cBackBrush = vNew(SystemBrush, COLOR_WINDOW);

	cSysCharHeight = gLineHeight(cFont);
	cSysCharWidth  = gAveCharWidth(cFont);

	cScalingMode = SM_1_PER_CHAR;

	cVSize = GetSystemMetrics(SM_CYSCREEN);
	cHSize = GetSystemMetrics(SM_CXSCREEN);

//	gDispose(stdoutStream);  You can't dispose of these with MSVCRT.LIB
//	gDispose(stderrStream);
//	gDispose(stdinStream);

	File;  //  must init File class first
	stdinStream_o = traceStream_o = NULL;

	stdoutStream_o = vNew(PopupWindow, "stdoutStream", 12, 45);
	gAutoShow(stdoutStream, 1);

	stderrStream_o = vNew(PopupWindow, "stderrStream", 12, 45);
	gAutoShow(stderrStream, 1);

	cHI = MakeProcInstance(KeyboardProc, hInstance);
#ifdef	WIN32
	cHook = SetWindowsHookEx(WH_KEYBOARD,
				 cHI,
				 hInstance,
				 GetCurrentThreadId());
	InitCommonControls();
#else
	cHook = SetWindowsHookEx(WH_KEYBOARD, cHI, hInstance, GetCurrentTask());
#endif

	cGlobals = gNew(StringDictionary);
#ifndef unix
	if (!cNoCtl3d)
		setup_Ctl3d();
#endif
	fix_disable_font_color();
}

void	_end_WDS(void)
{
	gDisposeExitObjects(Application);

	gDisposeModeless(MessageDispatcher);

	terminate_Ctl3d();

	free_objects();
}

typedef	BOOL   (WINAPI *wfun)(HINSTANCE);
typedef	BOOL   (WINAPI *sfun)(HWND, DWORD);

wfun	Ctl3dColorChange;
sfun	Ctl3dSubclassDlgEx;

static	void	setup_Ctl3d()
{
	wfun	reg, sub;
	
#ifdef	WIN32
	cCtl3d = gLoadLibrary(DynamicLibrary, "ctl3d32.dll");
#else
	cCtl3d = gLoadLibrary(DynamicLibrary, "ctl3d.dll");
#endif
	if (!cCtl3d)
		return;
	reg = (wfun) gGetProcAddress(cCtl3d, "Ctl3dRegister");
	sub = (wfun) gGetProcAddress(cCtl3d, "Ctl3dAutoSubclass");
	if (!reg  ||  !sub  ||  TRUE != reg(GhInstance))  {
		cCtl3d = gDispose(cCtl3d);
		return;
	}
	if (TRUE != sub(GhInstance)) {
		terminate_Ctl3d();
		return;
	}
	Ctl3dColorChange = (wfun) gGetProcAddress(cCtl3d, "Ctl3dColorChange");
	Ctl3dSubclassDlgEx = (sfun) gGetProcAddress(cCtl3d, "Ctl3dSubclassDlgEx");
}

static	void	terminate_Ctl3d()
{
	wfun	reg;

	if (!cCtl3d)
		return;
	reg = (wfun) gGetProcAddress(cCtl3d, "Ctl3dUnregister");
	if (reg)
		reg(GhInstance);
	cCtl3d = gDispose(cCtl3d);
	Ctl3dColorChange = NULL;
	Ctl3dSubclassDlgEx = NULL;
}

static	void	free_objects()
{
	if (cHook)
		UnhookWindowsHookEx(cHook);
	if (cHI)
		FreeProcInstance(cHI);

	gDispose(cCursor);
	gDispose(cIcon);
	gDispose(cFont);
	gDispose(cTextBrush);
	gDispose(cBackBrush);
	if (cArgPointers)
		gDeepDispose(cArgPointers);

	gFreeAll(DynamicLibrary);

#ifndef unix
	if (cUsingCOM)
//		CoUninitialize();
		OleUninitialize();
#endif

	gDeepDispose(cGlobals);

//	gDispose(stdoutStream);
//	gDispose(stderrStream);
	if (traceStream_o)
		gDispose(traceStream);
#ifdef	WIN32
	if (cMutex) {
		ReleaseMutex(cMutex);
		CloseHandle(cMutex);
	}
#endif
}

cmeth	gNoCtl3d()
{
	cNoCtl3d = 1;
	return self;
}

cmeth	gAddGlobal(char *key, val)
{
	ChkArgNul(val, 3);
	return gAddStr(cGlobals, key, val) ? val : NULL;
}

cmeth	gRemoveGlobal(char *key)
{
	return gRemoveStr(cGlobals, key) ? self : NULL;
}

cmeth	gGetGlobal(char *key)
{
	return gFindValueStr(cGlobals, key);
}

cmeth	gChangeGlobal(char *key, val)
{
	ChkArgNul(val, 3);
	return gChangeValueWithStr(cGlobals, key, val);
}

cmeth	gDisposeGlobal(char *key)
{
	return gDeepDisposeStr(cGlobals, key) ? self : NULL;
}

cmeth	HINSTANCE	gInstance()
{
	return cInstance;
}

cmeth	HINSTANCE	gPrevInstance()
{
	return cPrevInstance;
}

cmeth	int	gShow()
{
	return cCmdShow;
}

cmeth	char	*gCmdLine()
{
	return cCmdLine;
}

cmeth	gSetName(char *name)
{
	cNAME = gNewWithStr(String, name);
	return self;
}

cmeth	char	*gGetName()
{
	return cNAME ? gStringValue(cNAME) : NULL;
}

cmeth	int	gNumArgs()
{
	return cNumArgs;
}

cmeth	char	*gGetArg(int num)
{
	object	obj = gFindValueInt(cArgPointers, num);
	
	return obj ? gStringValue(obj) : "";
}

cmeth char gUsingAlternateColor()
{
	return cAltColor;
}

cmeth void gSetColorTheme(int r, int g, int b)
{
#if	WINVER >= 0x0500
	gSetBackBrush(self, gNewSolidBrush(SolidBrush, r, g, b));
#endif 
	cAltColor = 1;
	cAltRed = r;
	cAltBlue = b;
	cAltGreen = g;
}

cmeth	int	gGetColorTheme(int *r, int *g, int *b)
{
	*r = cAltRed;
	*g = cAltGreen;
	*b = cAltBlue;
	return cAltColor;
}

static	void	AppError(object self, char *msg)
{
	if (msg  &&  IsObj((object) msg))
		msg = gStringValue((object) msg);
	cErrorMessage = msg;
#ifndef __WINE__
	gStackDump(gGetTracer(StackTracer), msg);
#endif
	MessageBeep(-1);
	if (cAtFatalExit)
		disposeAtExit(cAtFatalExit);
	terminate_Ctl3d();
	free_objects();
	if (msg) {
		if (strlen(msg) > 255)
			msg[256] = '\0';  //  Windows 2000 won't display messages any longer!
		FatalAppExit(0, msg);
	} else
		FatalExit(-1);
//	MessageBox((HWND) 0, msg, "Error Window", MB_OK|MB_TASKMODAL);  
//	exit(1);
}

cmeth	char	*gGetErrorMessage()
{
	return cErrorMessage;
}

static	int	Trace(object self, int mode)
{
	if (!traceStream)  {
		traceStream_o = vNew(PopupWindow, "Trace Output", 12, 45);
		gAutoShow(traceStream_o, 1);
	}
	return (*cTrace)(self, mode);
}

cmeth	gSetCursor(obj)
{
	ChkArgTyp(obj, 2, Cursor);
	if (cCursor)
		gDispose(cCursor);
	cCursor = obj;
	return obj;
}

cmeth	gGetCursor()
{
	return cCursor;
}

cmeth	gSetIcon(obj)
{
	ChkArgTyp(obj, 2, Icon);
	if (cIcon)
		gDispose(cIcon);
	cIcon = obj;
	return obj;
}

cmeth	gGetIcon()
{
	return gCopy(cIcon);
}

cmeth	gSetFont(obj)
{
	ChkArgTyp(obj, 2, Font);
	if (cFont)
		gDispose(cFont);
	cFont = obj;
	return obj;
}

cmeth	gGetFont()
{
	return gCopy(cFont);
}

cmeth	gSetTextBrush(obj)
{
	ChkArgTyp(obj, 2, Brush);
	if (cTextBrush)
		gDispose(cTextBrush);
	cTextBrush = obj;
	return obj;
}

cmeth	gGetTextBrush()
{
	return gCopy(cTextBrush);
}

cmeth	gSetBackBrush(obj)
{
	ChkArgTyp(obj, 2, Brush);
	if (cBackBrush)
		gDispose(cBackBrush);
	cBackBrush = obj;
	return obj;
}

cmeth	gGetBackBrush()
{
	return gCopy(cBackBrush);
}

cmeth	gScaleToPixels(int *y, int *x, font)
{
	switch (cScalingMode)  {
	case SM_10_PER_SYSCHAR:
		if (y)
			*y = (long) cSysCharHeight * (long) *y / 10L;
		if (x)
			*x = (long) cSysCharWidth  * (long) *x / 10L;
		break;
	case SM_PIXELS:
		break;
	case SM_1_PER_CHAR:
		if (!font)
			font = cFont;
		if (y)
			*y = (long) gLineHeight(font)   * (long) *y;
		if (x)
			*x = (long) gAveCharWidth(font) * (long) *x;
		break;
	}
	return self;
}

cmeth	gScaleToCurrentMode : ScaleToCurrentMode (int *y, int *x, font)
{
	switch (cScalingMode)  {
	case SM_10_PER_SYSCHAR:
		if (y)
			*y = (long) *y * 10L / (long) cSysCharHeight;
		if (x)
			*x = (long) *x * 10L / (long) cSysCharWidth;
		break;
	case SM_PIXELS:
		break;
	case SM_1_PER_CHAR:
		if (!font)
			font = cFont;
		if (y)
			*y = (long) *y / (long) gLineHeight(font);
		if (x)
			*x = (long) *x / (long) gAveCharWidth(font);
		break;
	}
	return self;
}

cmeth	int	gSetScalingMode(int mode)
{
	int	r = cScalingMode;
	cScalingMode = mode;
	DEMO_CHK_MAX;
	return r;
}

cmeth	int	gGetScalingMode()
{
	return cScalingMode;
}

cmeth	gGetSize(int *height, int *width)
{
	*height = cVSize;
	*width  = cHSize;
	ScaleToCurrentMode(Application, height, width, cFont);
	return self;
}

#if 0
static	LRESULT	CALLBACK
MessageProc(int nCode, WPARAM wParam, LPARAM lParam) /*  for WH_MSGFILTER  */
{
	LPMSG	msg = (LPMSG) lParam;

	if ((nCode == MSGF_DIALOGBOX  ||  nCode == MSGF_MENU)  &&
	    msg->message == WM_KEYDOWN  &&  msg->wParam == VK_F1)
		gHelpInContext(HelpSystem);
	return CallNextHookEx(cHook, nCode, wParam, lParam);
}
#endif

static	LRESULT	CALLBACK
KeyboardProc(int nCode, WPARAM wParam, LPARAM lParam)  /*  for WH_KEYBOARD  */
{
	if (wParam == VK_F1  &&  nCode >= 0)
		gHelpInContext(HelpSystem);
	return CallNextHookEx(cHook, nCode, wParam, lParam);
}

cmeth	gQuitApplication(int result)
{
	PostQuitMessage(result);
	return self;
}

cmeth	int	gDisallowMultipleInstances(char *tag, char *msg)
{
	int	r;
#ifndef unix
#ifdef	WIN32
#if 0
	char	*cmd = GetCommandLine(), buf[MAX_PATH], *p;
	for (p=buf ; *cmd ; cmd++)
		if (*cmd == '\\'  ||  *cmd == '//')
			*p++ = ' ';
		else
			*p++ = toupper(*cmd);
	*p = '\0';
			
	cMutex = CreateMutex(NULL, TRUE, buf);
#else
	cMutex = CreateMutex(NULL, TRUE, tag);
#endif
	r = !!GetLastError();
#else
	r = !!cPrevInstance;
#endif
#endif
	if (r  &&  msg)
		MessageBox((HWND) 0, msg, "Message Window", MB_OK);  
	return r;
}

cmeth	int	gQuery(char *title, char *msg, UINT options)
{
	return MessageBox((HWND) 0, msg, title, options);  
}	

cmeth	gAddMessageObject(object obj)
{
	if (!cMessageObjects)
		cMessageObjects = gNew(LinkList);

	return gAddLast(cMessageObjects, gNewWithObj(LinkValue, obj));
}

cmeth	gResetMessageObjects()
{
	if (cMessageObjects)
		cMessageObjects = gDispose(cMessageObjects);

	return self;
}

cmeth	int	gMessageMode(int mode)
{
	int	pm = cMessageMode;

	if (mode > -1)
		cMessageMode = mode;

	return pm;
}

cmeth	gMessage(char *msg)
{
	object	seq, obj;
	
 	if (cMessageObjects)
		for ( seq = gSequence(cMessageObjects) ; obj = gNext(seq) ; )
			if (IsObj(gValue(obj))  &&  gIsKindOf(gValue(obj), Stream))
				vPrintf(gValue(obj), "%s\n", msg);
	
#ifdef	_WIN32
	if (cMessageMode)
		MessageBox((HWND) 0, msg, "Message Window", MB_OK | MB_TASKMODAL | MB_TOPMOST);  
#else
	if (cMessageMode)
		MessageBox((HWND) 0, msg, "Message Window", MB_OK | MB_TASKMODAL);  
#endif
	return self;
}

cmeth	gErrorMessage(char *msg)
{
#ifdef _WIN32
	MessageBox(NULL, msg, "Error Message Window", MB_OK | MB_TASKMODAL | MB_TOPMOST);  
#else
	MessageBox(NULL, msg, "Error Message Window", MB_OK | MB_TASKMODAL);  
#endif
	return self;
}

cmeth	char	*gGetBuf()
{
	if (!cBuf)
		cBuf = malloc(BUFSIZE);
	return cBuf;
}

cmeth	gUseCOM()
{
	if (cUsingCOM)
		return self;
#ifndef unix
//	cUsingCOM = SUCCEEDED(CoInitialize(NULL));
	cUsingCOM = SUCCEEDED(OleInitialize(NULL));
#endif
	return cUsingCOM ? self : NULL;
}

cmeth	int	gBufSize()
{
	return BUFSIZE;
}

cmeth	gCheckMessages()
{
	MSG	msg;
		
	while (PeekMessage(&msg, (HWND) 0, WM_TIMER, WM_TIMER, PM_REMOVE) ||
	       PeekMessage(&msg, (HWND) 0, WM_SETCURSOR, WM_SETCURSOR, PM_REMOVE) ||
	       PeekMessage(&msg, (HWND) 0, WM_PAINT, WM_PAINT, PM_REMOVE)) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return self;
}

static	int	dispose_popup(object popup)
{
	gDispose(popup);
	return 1;
}

cmeth	gDumpObjectsPopup(char *txt, object sd1, object sd2)
{
	object	seq, sa, diff, win;
	int	pm = gSetScalingMode(Application, SM_1_PER_CHAR);

	win = gNewPopupWindow(PopupWindow, "Object Dump", 15, 50);
	gLoadSystemFont(win, ANSI_FIXED_FONT);
	gSetMaxLines(win, 2000);
	gCompletionFunction(win, dispose_popup);
	vPrintf(win, "\n%s\n\n", txt);
	gShow(win);
	diff = sd2 ? gDumpObjectsDiff(Dynace, sd1, sd2) : sd1;
	for (seq=gSequence(diff) ; sa=gNext(seq) ; )
		vPrintf(win, "%5ld  %s\n", gLongValue(gValue(sa)), gStringKey(sa));
	if (sd2)
		gDeepDispose(diff);
	gSetScalingMode(Application, pm);
	return self;
}

cmeth	gDisposeAtExit(obj)
{
	if (!cAtExit)
		cAtExit = gNew(Set);
	return gAdd(cAtExit, obj);
}

cmeth	gRemoveAtExit(obj)
{
	return cAtExit ? gRemoveObj(cAtExit, obj) : NULL;
}

cmeth	gDisposeAtFatalExit(obj)
{
	if (!cAtFatalExit)
		cAtFatalExit = gNew(Set);
	return gAdd(cAtFatalExit, obj);
}

cmeth	gRemoveAtFatalExit(obj)
{
	return cAtFatalExit ? gRemoveObj(cAtFatalExit, obj) : NULL;
}

cmeth	int	gReturnAsTab(int mode)
{
	int	r = cReturnAsTab;
	if (mode >= 0)
		cReturnAsTab = mode;
	return r;
}

cmeth	gExecuteString(char *str)  // to allow link without scheme.d
{
	return self;
}

cmeth	int	gAutoDisposeExitObjects(int val)
{
	int	pv = cAutoDisposeAtExit;

	cAutoDisposeAtExit = val;
	return pv;
}

cmeth	gDisposeExitObjects()
{
	cExiting = 1;
	if (cAtExit)
		disposeAtExit(cAtExit);
	if (cAtFatalExit)
		disposeAtExit(cAtFatalExit);

	return NULL;
}

cmeth	int	gIsExiting()
{
	return cExiting;
}

static	void	disposeAtExit(object ae)
{
	object	obj;
	while (obj = gFirst(ae)) {
		gRemoveObj(ae, obj);
		if (IsObj(obj))
			gDeepDispose(obj);
	}
}

static	void	fix_disable_font_color(void)
{
	DWORD	ver = GetVersion();
	int	major = LOBYTE(LOWORD(ver));
	int	minor = HIBYTE(LOWORD(ver));
//	int	nt = ver < 0x80000000;
	int	dosmajor = HIBYTE(HIWORD(ver));
	int	dosminor = LOBYTE(HIWORD(ver));
/*
  In addition to the Win32 & Win16 docs I've discovered the following facts
  about Win16 GetVersion when operated under a 32 bit OS.

  OS           major    minor   dosmajor    dosminor
  ------       -----    -----   --------    --------
  NT 3.x         3       10        5           0
  NT 4.0         3       10        5           0
  95             3       95        7           0
*/


	//  The following test works for all environments except 16 bit apps under any NT

	if (major >= 4  ||
	    major == 3  &&  minor == 95  &&  dosmajor == 7  &&  dosminor == 0) {
		int		option = COLOR_GRAYTEXT;
		COLORREF	value  = RGB(0,0,0);
		SetSysColors(1, &option, &value);
	}
}

static	char	*read_cmdfile(char *file)
{
	FILE	*fp;
	char	buf[256], *r;

	if (!*file)
		return "";
	fp = fopen(file, "rt");
	r = fgets(buf, sizeof buf, fp);
	fclose(fp);
	if (r) {
		int	len = strlen(buf);
		if (buf[len-1] == '\n')
			buf[--len] = '\0';
		r = malloc(len+1);
		if (!r)
			gError(Application, "Out of memory");
		strcpy(r, buf);
		return r;
	} 
	return "";
}

static	void	processCmdLine()
{
	char	hold[256];
	char	*w = hold;
	char	*p = cCmdLine;

	cNumArgs = 0;

	if (!p  ||  !strlen(p))
		return;

	for ( ; isspace(*p) ; ++p);
	if (*p == '@') {
		p = cCmdLine = read_cmdfile(p+1);
		for ( ; isspace(*p) ; ++p);
		if (!*p)
			return;
	}
	cArgPointers = gNew(IntegerDictionary);
	while (*p) {
		if (*p == '"'  ||  *p == '\'')  {
			char	type = *p++;
			while (*p  &&  *p != type)  {
				if (*p == '\\'  &&  p[1])
					p++;
				*w++ = *p++;
			}
			if (*p)
				++p;
		} else if (*p  &&  isspace(*p)) {
			*w = '\0';
			if (w != hold)
				gAddInt(cArgPointers, cNumArgs++, gNewWithStr(String, w=hold));
			for ( ; isspace(*p) ; ++p);
		} else
			while (*p  &&  !isspace(*p)) {
				if (*p == '\\'  &&  p[1])
					p++;
				*w++ = *p++;
			}
	}
	*w = '\0';
	if (w != hold)
		gAddInt(cArgPointers, cNumArgs++, gNewWithStr(String, hold));
}

static	void	class_init()
{
	cInstance     = GhInstance;
	cPrevInstance = GhPrevInstance;
	cCmdLine      = GlpszCmdLine;
	cCmdShow      = GcmdShow;
	cAutoDisposeAtExit = 1;
	cMessageMode = 1;
	processCmdLine();
}

/*  to prevent link problems  */

#ifndef unix

#ifndef	WIN32  //  this trap breaks console WDS but is required for WIN16
int	printf(const char *fmt, ...)
{
	char	buf[512];
	va_list	rest;
	va_start(rest, fmt);
	vsprintf(buf, fmt, rest);
	vError(Application, "printf called with %s", buf);
	return 0;
}
#endif


#endif

//  generics not defined or used but referenced
//  (used with scheme or the resource stuff)

cmeth	gUpdatePosition()
{
	gShouldNotImplement(self, "gUpdatePosition");
	return self;
}

cmeth	char	*gFunctionName(obj)
{
	gShouldNotImplement(self, "gFunctionName");
	return NULL;
}

cmeth	char	*gNamespaceName(obj, char *ns)
{
	gShouldNotImplement(self, "gNamespaceName");
	return NULL;
}

cmeth	gExecuteInNamespace(char *ns, char *str)
{
	gShouldNotImplement(self, "gExecuteInNamespace");
	return self;
}

#define	MAX_URL		1024

cmeth	gOpenURL(char *urlin)
{
#ifdef	_WIN32
	char	url[MAX_URL];

	strcpy(url, urlin ? urlin : "");
	if (*url == '/' && url[1] == '/') {
		*url = '\\';
		url[1] = '\\';
	}

	ShellExecute(NULL, "open", url, NULL, NULL, SW_SHOWNORMAL); 
#endif
	return self;
}

cmeth	gOpenURLWait(char *urlin)
{
#ifdef	_WIN32
	char tempPath[MAX_URL], exePath[MAX_URL], commandLine[MAX_URL];
	char url[MAX_URL];
	HINSTANCE hInstVal;
	long sresult;
	STARTUPINFO sInfo;
	PROCESS_INFORMATION pInfo;
	object extObj;
	char *ext;

	strcpy(url, urlin ? urlin : "");
	if (*url == '/' && url[1] == '/') {
		*url = '\\';
		url[1] = '\\';
	}

	GetTempPath(sizeof(tempPath), tempPath);
	// this call is necessary to find the full path to the default browser (ie IE)
	// which can be in one of several default installation locations
	// depending on version, OS, and OEM retailer install
	hInstVal = FindExecutable(url, tempPath, exePath);

	extObj = gNewWithStr(String, strrchr(url, '.'));
	ext = gStringValue(gToUpper(extObj));

	if (!_stricmp(ext, ".HTML") || !_stricmp(ext, ".HTM")) {
		if (strstr(_strlwr(exePath), "iexplore.exe")) 
			sprintf(commandLine, "-nohome %s", url);
		else
			sprintf(commandLine, " -url %s", url);
	} else
		sprintf(commandLine, " %s", url);
	
	memset(&sInfo, 0, sizeof sInfo);
	memset(&pInfo, 0, sizeof pInfo);

	sInfo.cb = sizeof sInfo;
	sInfo.dwFlags = STARTF_USESHOWWINDOW;
	sInfo.wShowWindow = SW_SHOWNORMAL;

	sresult = CreateProcess(exePath,
		commandLine,
		NULL, NULL, 0,
		NORMAL_PRIORITY_CLASS,
		NULL, NULL,
		&sInfo, &pInfo);

	if (sresult != 0) {
		if (pInfo.hProcess)
			WaitForSingleObject(pInfo.hProcess, INFINITE);

		CloseHandle(pInfo.hProcess);
		CloseHandle(pInfo.hThread);
	}
	gDispose(extObj);
#endif
	return self;
}

// function to open the default windows app and print the specified file
cmeth	gPrintURL(char *urlin)
{
#ifdef	_WIN32
	char	url[MAX_PATH];

	strcpy(url, urlin ? urlin : "");
	if (*url == '/' && url[1] == '/') {
		*url = '\\';
		url[1] = '\\';
	}

	ShellExecute(NULL, "print", url, NULL, NULL, SW_HIDE);
#endif
	return self;
}


#ifndef	_WIN32

static	DWORD	timeout;
static	int	sleeping;

static	long	process_wm_create(object win, HWND hwnd, UINT mMsg, WPARAM wParam, LPARAM lParam)
{
	SetTimer(hwnd, 314, (UINT) timeout, NULL);
	return 0L;
}

static	long	process_wm_timer(object win, HWND hwnd, UINT mMsg, WPARAM wParam, LPARAM lParam)
{
	if (wParam == 314)
		sleeping = 0;
	return 0L;
}

void	Sleep(DWORD millisecs)
{
	object	wind;
	MSG	msg;
	HWND	hwnd;

	if (!millisecs) {
		Yield();
		return;
	}
	timeout = millisecs;
	sleeping = 1;
	wind = vNew(PopupWindow, "Sleep", 4, 20);
	gSetStyle(wind, 0);
	gAddHandlerAfter(wind, (unsigned) WM_TIMER, process_wm_timer);
	gAddHandlerAfter(wind, (unsigned) WM_CREATE, process_wm_create);
	gShow(wind);
	hwnd = gHandle(wind);

	while (sleeping  &&  GetMessage(&msg, hwnd, 0, 0))  {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	
	gDispose(wind);
}

#endif

cmeth	int	gIsTerminalServer()
{
	return GetSystemMetrics(0x1000);
//	return GetSystemMetrics(SM_REMOTESESSION);
}

cmeth	gGetLogicalDrives()
{
	object	rval = NULL;
	DWORD	drvs = GetLogicalDrives();
	char	ch = 'A';

	while (drvs) {
		if (drvs % 2) {
			if (!rval)
				rval = gNew(LinkObject);
			gAddLast(rval, gNewWithChar(Character, ch));
		}
		ch++;
		drvs /= 2;
	}
	return rval;
}

cmeth	int	gGetDriveType(char drive_letter)
{
	char	buf[10];

	sprintf(buf, "%c:", drive_letter);

	return GetDriveType(buf);
}

cmeth	char	*gGetDriveName(char drive_letter)
{
	static	char	devName[100];
	int		devSz = 100;
	char		buf[10];
	char		*rval;

	sprintf(buf, "%c:", drive_letter);
	switch (GetDriveType(buf)) {
	case DRIVE_UNKNOWN:	// The drive type cannot be determined.
		rval = "Undetermined";
		break;
	case DRIVE_NO_ROOT_DIR:	// The root path is invalid. For example, no volume is mounted at the path.
		rval = "Invalid";
		break;
	case DRIVE_REMOVABLE:	// The disk can be removed from the drive.
		rval = "Removable";
		break;
	case DRIVE_FIXED:	// The disk cannot be removed from the drive.
		rval = "Local Disk";
		break;
	case DRIVE_CDROM:	// The drive is a CD-ROM drive.
		rval = "CD-ROM";
		break;
	case DRIVE_RAMDISK:	// The drive is a RAM disk. 
		rval = "RAM Disk";
		break;
	case DRIVE_REMOTE:	// The drive is a remote (network) drive.
		switch (WNetGetConnection(buf, devName, &devSz)) {
		case NO_ERROR:
			rval = devName;
			break;
		default:
			rval = "Network";
			break;
		}
		break;
	default:
		rval = "Unknown";
		break;
	}
	return rval;
}

cmeth	char	*gGetDriveUNC(char drive_letter)
{
	static	char		uncName[100];
	int			uncSz = 100;
	char			buf[10];
	TCHAR			uncBuff[1000];
	REMOTE_NAME_INFO	*rni = (REMOTE_NAME_INFO *) &uncBuff;
	DWORD			rnisz = 1000;
	char			*rval = NULL;

	sprintf(buf, "%c:", drive_letter);
	if (WNetGetUniversalName(buf, REMOTE_NAME_INFO_LEVEL, (void *) rni, &rnisz) == NO_ERROR &&
		rni->lpUniversalName) {
		strcpy(uncName, rni->lpUniversalName);
		rval = uncName;
	}
	return rval;
}


cmeth gSetLanguage(int lang)
{
	cLanguage=lang;
	return self;
}

cmeth int gLanguage()
{
	return cLanguage;
}

cmeth char ** gLanguages()
{
	return languages;
}




