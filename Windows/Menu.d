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



defclass  Menu  {
	HMENU	iHMenu;		/*  handle to menu		*/
	iActions;		/*  dictionary of actions  	*/
	iValues;		/*  dictionary of values (for squeak)  */
	iNext;			/*  list of pushed menus	*/
	iParent;
	
	iMouseFunctions;
	object	iResFile;
class:
	ifun	cMouseFunction[6];
	ifun	cAccessMode;
};

imeth	gSetMode : setMode(unsigned id, unsigned mode)
{
	unsigned oldMode=EnableMenuItem(iHMenu, id, (~MF_BYPOSITION & mode) | MF_BYCOMMAND);
	if (iParent && gIsKindOf(iParent, Window)&& oldMode!=mode)
		gRedrawMenu(iParent);
	return self;
}

imeth	int	gSetCheckMark(unsigned id, int val)
{
#ifdef	WIN32
	return CheckMenuItem(iHMenu, id, val ? MF_CHECKED : MF_UNCHECKED);
#else
	return -1;
#endif
}

// Yanghui:
////////////////////////////////////////////////////////////////////////////////////////
// gEnableMenuItem:
//
// nIDEnableItem:  Specifies the menu item to be enabled, as determined by nEnable.
//
// nEnable:        Specifies the action to take. It can be a combination of MF_DISABLED, 
//                 MF_ENABLED, or MF_GRAYED, with MF_BYCOMMAND or MF_BYPOSITION. These 
//                 values can be combined by using the bitwise OR operator.  Don't 
//					combine the values!  Just use one at a time.  Yangui didn't read the
//					API.
//
// Return Values:  The return value specifies the previous state of the menu item (it is 
//                 either MF_DISABLED, MF_ENABLED, or MF_GRAYED). If the menu item does 
//                 not exist, the return value is -1.
//
////////////////////////////////////////////////////////////////////////////////////////
imeth unsigned  gEnableMenuItem(unsigned nIDEnableItem, unsigned nEnable)
{
	unsigned	rval = EnableMenuItem(iHMenu, nIDEnableItem, nEnable);

	if (iParent && gIsKindOf(iParent, Window) && rval!=nEnable)
		gRedrawMenu(iParent);
	return rval;
}

// Yanghui


private	imeth	pCheckMenuItems(object self, HMENU hMenu)
{
	int	i;
	int	num = GetMenuItemCount(hMenu);
	UINT	id;
	char changed=0;

	for (i = 0; i < num; i++) {
		id = GetMenuItemID(hMenu, i);
		if (id == -1)
			pCheckMenuItems(self, GetSubMenu(hMenu, i));
		else if (id  &&  cAccessMode(self, id)) {
			if (EnableMenuItem(iHMenu, id, (~MF_BYPOSITION & MF_GRAYED) | MF_BYCOMMAND)!=MF_GRAYED)
				changed=1;
		} else if (id) {
			if (EnableMenuItem(iHMenu, id, (~MF_BYPOSITION & MF_ENABLED) | MF_BYCOMMAND)!=MF_ENABLED)
				changed=1;
		}
	}

	if (changed  &&  iParent  &&  gIsKindOf(iParent, Window))
		gRedrawMenu(iParent);

	return self;
}

imeth	gUpdateAccessMode()
{
	if (!cAccessMode)
		return self;
	
	return pCheckMenuItems(self, iHMenu);
}

/*  This method should only be used internally  (by subclasses of Menu)  */

cvmeth	vNew(HMENU hmenu)
{
	object	obj = gNew(super);
	ivType	*iv = ivPtr(obj);

	iActions = gNewWithInt(IntegerDictionary, 19);
	iValues = gNewWithInt(IntegerDictionary, 19);
	iMouseFunctions = gNewWithInt(IntegerDictionary, 19);
	iHMenu = hmenu;

	if (cAccessMode)
		pCheckMenuItems(obj, iHMenu);

//	HC_NEW(MENU_HANDLE_CACHE, iHMenu, obj);
	return obj;
}

imeth	object	gDispose()
{
	gDeepDispose(iActions);
	gDeepDispose(iValues);
	gDeepDispose(iMouseFunctions);
	DestroyMenu(iHMenu);
//	HC_DELETE(MENU_HANDLE_CACHE, iHMenu);
	if (iResFile)
		gDispose(iResFile);
	return gDispose(super);
}

imeth	object	gDeepDispose()
{
	object	nxt;

	while (self)  {
		DestroyMenu(iHMenu);
		gDeepDispose(iActions);
		gDeepDispose(iValues);
		gDeepDispose(iMouseFunctions);
//		HC_DELETE(MENU_HANDLE_CACHE, iHMenu);
		nxt = iNext;
		gDispose(super);

		if (self = nxt)
			iv = ivsPtr;
	}
	return NULL;
}

imeth	gAssociate(int count, void (*fun)())
{
	object	p, r;
	int	i;

	r = gAddInt(iActions, count, p=gNewWithPtr(Pointer, fun));
	if (!r)  {
		gDeepDisposeInt(iActions, count);
		gAddInt(iActions, count, p);
	}

	p = NULL;
	for (i = 0; i < 6; i++)
		if (cMouseFunction[i]) {
			if (!p)
				p = vNew(PointerArray, 1, 6);
			vChangeValue(p, cMouseFunction[i], i);
		}
	if (p) {
		r = gAddInt(iMouseFunctions, count, p);
		if (!r)  {
			gDeepDisposeInt(iMouseFunctions, count);
			gAddInt(iMouseFunctions, count, p);
		}
	}
	
	return self;
}

imeth	gAssociateLongWithID(int count, long val)
{
	object	p, r;

	r = gAddInt(iValues, count, p=gNewWithLong(LongInteger, val));
	if (!r)  {
		gDeepDisposeInt(iValues, count);
		gAddInt(iValues, count, p);
	}
	return self;
}

/*  the return type of ofun is only used to be consistant  */

imeth	ofun	gMenuFunction(int c)
{
	object	fo;

	fo = gFindValueInt(iActions, c);
	return (ofun) (fo ? gPointerValue(fo) : NULL);
}

imeth	long	gLongAssociationWithID(int c)
{
	object	fo;

	fo = gFindValueInt(iValues, c);
	return fo ? gLongValue(fo) : 0L;
}

imeth	HANDLE	gHandle()
{
	return (HANDLE) iHMenu;
}

imeth	gPush(menu)
{
	ChkArgNul(menu, 2);
	iNext = menu;
	return self;
}

imeth	gPop()
{
	object	menu = iNext;
	gDispose(self);
	return menu;
}

static	int	get_index(WPARAM p)
{
	if (p & MK_RBUTTON)
		if (p & MK_SHIFT)
			return 2;
		else if (p & MK_CONTROL)
			return 4;
		else
			return 0;
	else if (p & MK_LBUTTON)
		if (p & MK_SHIFT)
			return 3;
		else if (p & MK_CONTROL)
			return 5;
		else
			return 1;
	else
		return -1;
}

imeth	ofun	gSetMenuMouseFunction(unsigned id, unsigned button, ifun fun)
{
	int	i = get_index(button);
	ifun	org = NULL;

	if (i >= 0) {
		object	fo = gFindValueInt(iMouseFunctions, id);

		if (fo) {
			org = (ifun) vPointerValue(fo, i);
			vChangeValue(fo, fun, i);
		} else if (fun) {
			object	r, p = vNew(PointerArray, 1, 6);

			vChangeValue(p, fun, i);
			r = gAddInt(iMouseFunctions, id, p);
			if (!r)  {
				gDeepDisposeInt(iMouseFunctions, id);
				gAddInt(iMouseFunctions, id, p);
			}
		}
	}
	return (ofun) org;
}

imeth	ifun	gGetMouseFunction(unsigned id, WPARAM p)
{
	int	i = get_index(p);
	
	if (i >= 0) {
		object	fo = gFindValueInt(iMouseFunctions, id);

		if (fo)
			return (ifun) vPointerValue(fo, i);
	}
	return NULL;
}

cmeth	ofun	gSetMouseFunction(unsigned button, ifun fun)
{
	int	i = get_index(button);
	ifun	org = NULL;

	if (i >= 0) {
		org = cMouseFunction[i];
		cMouseFunction[i] = fun;
	}
	return (ofun) org;
}

cmeth	ifun	gSetAccessModeFunction(ifun f)
{
	ifun	org = cAccessMode;
	cAccessMode = f;
	return org;
}

imeth	gSetParent(object pwnd)
{
	object	op = iParent;
	
	iParent = pwnd;
	return op;
}

imeth	gGetParent()
{
	return iParent;
}

imeth	gSetMenuFileName(char *fname)
{
	if (iResFile)
		gChangeStrValue(iResFile, fname);
	else
		iResFile = gNewWithStr(String, fname);
	return self;
}

imeth	char	*gMenuFileName()
{
	return iResFile ? gStringValue(iResFile) : "";
}

imeth void gSetColorTheme(int r, int g, int b)
{
#if	WINVER >= 0x0500
	MENUINFO mi;
	object brobj;
	HBRUSH brush;

	mi.cbSize = sizeof(MENUINFO);

	GetMenuInfo(iHMenu, &mi);

	mi.fMask = MIM_APPLYTOSUBMENUS | MIM_BACKGROUND;

	//mi.hbrBack=GetSysColorBrush(COLOR_INACTIVECAPTION);

	mi.hbrBack = gHandle(gGetBackBrush(Application));
	SetMenuInfo(iHMenu, &mi);
#else
	;
#endif 
}







