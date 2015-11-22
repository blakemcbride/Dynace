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




#include "logfile.h"
#include "hdlcache.h"

defclass  CustomControl : Control  {
	HWND	iHCtl;		/*  handle to edit control  	*/
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	int	(*iAcf)();	/*  aux checking function	*/

	/*  help info associated with a control   */

	iTopic;			/*  help topic associated with control */
	char	*iPrevTopic;	/*  previous topic	*/
};


#include <ctype.h>


private	imeth	LRESULT	process_wm_setfocus(object	self, 
					    HWND	hwnd, 
					    UINT	mMsg, 
					    WPARAM	wParam, 
					    LPARAM	lParam)
{
	if (iTopic)
		iPrevTopic = gSetTopic(HelpSystem, gStringValue(iTopic));
	return 0L;
}

private	imeth	LRESULT	process_wm_killfocus(object	self, 
					     HWND	hwnd, 
					     UINT	mMsg, 
					     WPARAM	wParam, 
					     LPARAM	lParam)
{
	if (iTopic)
		gSetTopic(HelpSystem, iPrevTopic);
	return 0L;
}

cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);
	iCtlID = ctlID;
	iDlg = dlg;

	/*  Init message handlers  */

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	return obj;
}

cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewCont(super, name, "static", parent);
	ivType	*iv = ivPtr(obj);

	iDlg = parent;
	iCtlID = ctlID;

	gSetStyle(obj, WS_VISIBLE);

	/*  Init message handlers  */

	gAddHandlerAfter(obj, (unsigned) WM_SETFOCUS, process_wm_setfocus);

	gAddHandlerAfter(obj, (unsigned) WM_KILLFOCUS, process_wm_killfocus);

	return obj;
}

imeth	gSetStyle(DWORD style)
{
	style = WS_CHILD | style & ~(WS_OVERLAPPED | WS_POPUP);
	return gSetStyle(super, style);
}

imeth	int	gShow()
{
	gShow(super);
	iHCtl = gHandle(super);
	gSubclassWindow(self, iHCtl);

	gInitialize(super, (HWND)0, NULL);
	return 0;
}

imeth	gInitialize(HWND hDlg, dlg)
{
	iDlg  = dlg;
	iHCtl = GetDlgItem(hDlg, iCtlID);
	if (!iHCtl) {
		char	buf[100];
		sprintf(buf, "CustomControl control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
	gSubclassWindow(self, iHCtl);
	return gInitialize(super, hDlg, dlg);
}

imeth	object	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);
	if (iTopic)
		gDispose(iTopic);
	if (IsObj((object) iAcf))
		gDispose((object) iAcf);
	return gDispose(super);
}

imeth	gReleaseHandle()
{
	if (iHCtl) {
		HC_DELETE(WINDOW_HANDLE_CACHE, iHCtl);
		iHCtl = 0;
	}
	return self;
}	

imeth	int	gCheckValue()
{
	char	*buf = gGetBuf(Application);

	if (iAcf) {
		int	r = 0;

		if (SchemeClassSurrogate  &&  IsObj((object)iAcf)  &&  ClassOf(iAcf) == String) {
			char	cmd[100], ns[80];
			object	ret;

			sprintf(cmd, "(%s (int->object %lld) (int->object %lld))",
				gFunctionName(SchemeClassSurrogate, (object)iAcf),
				(long long) self, (long long) 0);
			ret = gExecuteInNamespace(SchemeClassSurrogate,
						  gNamespaceName(SchemeClassSurrogate, (object)iAcf, ns), 
						  cmd);
			if (IsObj(ret)) {
				if (r = ClassOf(ret) == String)
					strcpy(buf, gStringValue(ret));
				gDispose(ret);
			}
		} else if (JavaScriptClassSurrogate  &&  IsObj((object)iAcf)  &&  ClassOf(iAcf) == JavaScriptString) { 
			object	ret;
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%lld\"), StringToObject(\"%lld\"))", gStringValue((object)iAcf), (long long) self, (long long) 0);
			ret = gExecuteString(JavaScriptClassSurrogate, cmd);
			if (IsObj(ret)) {
				if (r = ClassOf(ret) == String)
					strcpy(buf, gStringValue(ret));
				gDispose(ret);
			}
		} else if (JavaCallbackClassSurrogate  &&  IsObj((object)iAcf)  &&  ClassOf(iAcf) == JavaCallbackClassSurrogate) {
			object msg = gPerformJavaCheckValueCallback((object)iAcf, self, NULL);
			if (msg) {
				r = 1;
				strcpy(buf, gStringValue(msg));
				gDispose(msg);
			}
		} else
			r = iAcf(self, NULL, buf);
		if (r) {
			if (*buf)
//				MessageBox(gHandle(iDlg), buf, "Error Message Window", MB_OK);  
				gErrorMessage(Application, buf);
			SetFocus(iHCtl);
			return 1;	/*  error  */
		}
	}
	return 0;
}

imeth	gCheckFunction(int (*fun)())
{
	if (IsObj((object) iAcf))
		gDispose((object) iAcf);
	iAcf = fun;
	return self;
}

imeth	char	*gSetTopic(char *topic)
{
	if (iTopic)
		if (topic  &&  *topic)
			gChangeStrValue(iTopic, topic);
		else
			iTopic = gDispose(iTopic);
	else if (topic  &&  *topic)
		iTopic = gNewWithStr(String, topic);
	return topic;
}

imeth	char	*gGetTopic()
{
	return iTopic ? gStringValue(iTopic) : NULL;
}

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHCtl;
}

imeth	unsigned  gGetCtlID()
{
	return iCtlID;
}

imeth	gDialog, gGetParent ()
{
	return iDlg;
}









