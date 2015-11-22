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




#include "hdlcache.h"


defclass TreeView : Control {
	HWND	iHCtl;		/*  handle to TreeView control 	*/
	UINT	iCtlID;		/*  control ID			*/
	iDlg;			/*  dialog object		*/
	iItems;
	int	(*iAcf)();	/*  aux checking function	*/
	int	iInternalMessage;
	int	(*iSelFun)();	// selection changed function
	int	(*iExpFun)();
	int	iAlphabetize;
	iImageList;
};

#include <commctrl.h>


cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, name);
	ivType	*iv = ivPtr(obj);
	iCtlID = ctlID;
	iDlg = dlg;
	iItems = gNew(Set);
	return obj;
}

imeth	gAddTVItem(parent, char *txt)
{
	object	item = gNewTVItem(TreeViewItem, self, parent, 0, iAlphabetize, txt);
	if (!parent)
		gAdd(iItems, item);
	return item;
}

imeth	gAddTVItemWithImage(parent, int image, char *txt)
{
	object	item = gNewTVItem(TreeViewItem, self, parent, image, iAlphabetize, txt);
	if (!parent)
		gAdd(iItems, item);
	return item;
}

imeth	gInitialize(HWND hDlg, dlg)
{
	iDlg  = dlg;
	iHCtl = GetDlgItem(hDlg, iCtlID);
	if (!iHCtl) {
		char	buf[100];
		sprintf(buf, "TreeView control %s (%d) not found.", gName(self), iCtlID);
		gError(self, buf);
	}

	/*  The following is needed to kill the auto-tips windows displays on long items (those
	    which extend beyond the contol boundry) for regressing testing software.  The tip
	    confuses the testing software.  */
	{
		static	int	got_env = 0;
		static	char	*env = NULL;
		HWND	tt;

		if (!got_env) {
			env = getenv("NOTOOLTIPS");
			got_env = 1;
		}
		if (env  && (tt = TreeView_SetToolTips(iHCtl, NULL)))
			DestroyWindow(tt);
	}

	if (iImageList)
		TreeView_SetImageList(iHCtl, gHandle(iImageList), TVSIL_NORMAL);
	HC_NEW(WINDOW_HANDLE_CACHE, iHCtl, self);
//	gSubclassWindow(self, iHCtl);
	return gInitialize(super, hDlg, dlg);
}

imeth	gDispose, gDeepDispose ()
{
	gReleaseHandle(self);
	gDeepDispose(iItems);
	if (IsObj((object) iAcf))
		gDispose((object) iAcf);
	if (iImageList)
		gDispose(iImageList);
	return gDispose(super);
}

imeth	gReleaseHandle()
{
	object	seq, obj;

	for (seq=gSequence(iItems) ; obj=gNext(seq) ; )
		gReleaseHandle(obj);
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
			int	res;
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
			sprintf(cmd, "%s(StringToObject(\"%lld\"), %lld)", gStringValue((object)iAcf), (long long) self, (long long) 0);
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

static	object	get_tvi(HANDLE h)
{
	object	tvi;
	if (h) {
		tvi = gGetObject(HandleCache, WINDOW_HANDLE_CACHE, h);
		if (tvi  &&  (!IsObj(tvi)  ||  ClassOf(tvi) != TreeViewItem))
			tvi = NULL;
	} else
		tvi = NULL;
	return tvi;
}

imeth	int	gProcessNotify(LPARAM lParam)
{
	NM_TREEVIEW *p = (NM_TREEVIEW *) lParam;
	object	ftvi, ttvi;

	if (!iInternalMessage) {
		if (p->hdr.code == TVN_SELCHANGED  &&  iSelFun) {
			ftvi = get_tvi(p->itemOld.hItem);
			ttvi = get_tvi(p->itemNew.hItem);
			iSelFun(self, ftvi, ttvi);
		}
		if (p->hdr.code == TVN_ITEMEXPANDED  &&  iExpFun) {
			ttvi = get_tvi(p->itemNew.hItem);
			if (ttvi  &&  (p->action == TVE_EXPAND  ||  p->action == TVE_COLLAPSE))
				iExpFun(self, ttvi, p->action == TVE_EXPAND);
		}
	}
	return FALSE;
}

imeth	ifun	gSetSelFun(ifun fun)
{
	ifun	pfun = iSelFun;
	iSelFun = fun;
	return pfun;
}

imeth	ifun	gSetExpFun(ifun fun)
{
	ifun	pfun = iExpFun;
	iExpFun = fun;
	return pfun;
}

imeth	gDeselect()
{
	iInternalMessage++;
	if (iHCtl)
		TreeView_SelectItem(iHCtl, (HANDLE) 0);
	iInternalMessage--;
	return self;
}

imeth	gGetSelection()
{
	HWND	h = (HWND) 0;

	if (iHCtl)
		h = (HWND) TreeView_GetSelection(iHCtl);
	return h ? gGetObject(HandleCache, WINDOW_HANDLE_CACHE, h) : NULL;
}

imeth	gFirst()
{
	HWND	h = (HWND) 0;

	if (iHCtl)
		h = (HWND) TreeView_GetRoot(iHCtl);
	return h ? gGetObject(HandleCache, WINDOW_HANDLE_CACHE, h) : NULL;
}

imeth	gRemoveAll()
{
	iInternalMessage++;
	if (iHCtl) {
		TreeView_SelectItem(iHCtl, (HANDLE) 0);
//		TreeView_DeleteAllItems(iHCtl);    the gDeepDisposeAllNodes does this
	}
	gDeepDisposeAllNodes(iItems);
	iInternalMessage--;
	return self;
}

static	int	CALLBACK	sortOriginalOrder(LPARAM tvi1, LPARAM tvi2, LPARAM self)
{
	int	rval = gLongValue(gPropertyGet((object) tvi1, "order")) - gLongValue(gPropertyGet((object) tvi2, "order"));

	if (!rval)
		rval = gLongValue(gPropertyGet((object) tvi1, "suborder")) - gLongValue(gPropertyGet((object) tvi2, "suborder"));
	return rval;
}

imeth	gResort()
{
	if (iHCtl) {
		object	sel = gGetSelection(self);
		iInternalMessage++;
		TreeView_SelectItem(iHCtl, (HANDLE) 0);
		if (iAlphabetize)
			TreeView_SortChildren(iHCtl, NULL, 1);
		else {
			TVSORTCB	psort;

			psort.hParent = NULL;
			psort.lpfnCompare = sortOriginalOrder;
			psort.lParam = (LPARAM) self;
			TreeView_SortChildrenCB(iHCtl, &psort, 1);
		}
		iInternalMessage--;
		gSelect(sel);
//		TreeView_SelectSetFirstVisible(iHCtl, TreeView_GetSelection(iHCtl));
	}
	return self;
}

imeth	int	gInternalMessage(int n)
{
	iInternalMessage += n;
	return iInternalMessage;
}

imeth	gSetImageList(object imageList)
{
	if (iImageList)
		gDispose(iImageList);
	iImageList = imageList;
	if (iHCtl  &&  iImageList)
		TreeView_SetImageList(iHCtl, gHandle(iImageList), TVSIL_NORMAL);
	return self;
}

imeth	gNewImageList(int cx, int cy, unsigned flags, int numb, int grow)
{
	object	il = gNewImageList(ImageList, cx, cy, flags, numb, grow);
	gSetImageList(self, il);
	return il;
}

imeth	int	gLoadBitmap(unsigned resID)
{
	return iImageList ? gLoadBitmap(iImageList, resID) : -1;
}

imeth	gAlphabetize()
{
	iAlphabetize = 1;
	return self;
}

imeth	gAlphabetizeOff()
{
	iAlphabetize = 0;
	return self;
}




