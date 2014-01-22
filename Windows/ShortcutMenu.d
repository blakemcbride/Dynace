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

defclass  ShortcutMenu : Menu  {
	int	iCount;		/*  next automatic ID		*/
	object	iPopups;	/*  linked list of popup menus  */
	int	iSubmenu;
};


typedef	long	(*lfun)();

private	imeth	pInitInstance(object self, int submenu, char *fname)
{
	iSubmenu = submenu;
	iPopups = gNew(LinkObject);
	if (fname && *fname)
		gSetMenuFileName(self, fname);
	return self;
}

cvmeth	vNew()
{
	return pInitInstance(vNew(super, CreatePopupMenu()), -1, NULL);
}

cmeth	gLoadShortcutMenuStr : LoadStr (char *name, int submenu)
{
	HMENU	h;

	h = LoadMenu(gInstance(Application), name);
	if (!h)
		return NULL;

	return pInitInstance(vNew(super, h), submenu, NULL);
}

cmeth	gLoadShortcutMenuFromFile(char *file, unsigned id, int submenu)
{
#ifdef	WIN32
	HMENU	h = ResourceLoadMenu(file, MAKEINTRESOURCE(id));
	if (h)
		return pInitInstance(vNew(super, h), submenu, file);
	else
#endif
		vError(self, "gLoadResourceFromFile: Can't find resource %u in file %s", id, file);
	return NULL;
}

cmeth	gLoadShortcutMenu(unsigned id, int submenu)
{
	return LoadStr(self, (char *) MAKEINTRESOURCE(id), submenu);
}

imeth	object	gDispose()
{
	gDeepDispose(iPopups);
	return gDispose(super);
}

imeth	object	gDeepDispose()
{
	gDeepDispose(iPopups);
	return gDeepDispose(super);
}

imeth	int	gAddMenuOption(char *title, void (*fun)())
{
	AppendMenu(gHandle(self), MF_STRING, ++iCount, title);
	gAssociate(self, iCount, fun);
	return iCount;
}

imeth	gAddPopupMenu : add_popup_menu (char *title, menu)
{
	ChkArg(menu, 2);
	AppendMenu(gHandle(self), MF_POPUP, (UINT) gHandle(menu), title);
	gAddLast(iPopups, menu);
	return self;
}

imeth	gAddSubMenu(char *title)
{
	object	menu = vNew(PopupMenu, self);
	
	add_popup_menu(self, title, menu);
	
	return menu;
}

imeth	int	gNextID()
{
	return ++iCount;
}

imeth	gTopMenu()
{
	return self;
}

imeth	gAddSeparator()
{
	AppendMenu(gHandle(self), MF_SEPARATOR, 0, 0);
	return self;
}

imeth	int	gPerform()
{
	POINT		p;
	unsigned	id;
	HMENU		hmenu = gHandle(self);
	object		wind = gGetParent(self);

	if (iSubmenu >= 0)
		hmenu = GetSubMenu(hmenu, iSubmenu); 
	GetCursorPos(&p);
	
	if (id = TrackPopupMenuEx(hmenu,
				  TPM_LEFTALIGN | TPM_TOPALIGN | TPM_LEFTBUTTON | TPM_RETURNCMD,
				  p.x,
				  p.y,
				  gHandle(wind),
				  NULL)) {
		lfun	fp = (lfun) gMenuFunction(self, id);

		if (fp) {
			if (IsObj((object)fp)  &&  ClassOf(fp) == JavaCallbackClassSurrogate) {
				object	callback = (object)fp;
				BOOL	bErr;
			
				gPerformJavaMenuCallback(callback, wind, id);
			} else if (SchemeClassSurrogate  &&  IsObj((object)fp)  &&  ClassOf(fp) == String) {
				char	cmd[100], ns[80];

				sprintf(cmd, "(%s (int->object %ld) %u)",
					gFunctionName(SchemeClassSurrogate, (object)fp),
					(long) wind, id);
				gExecuteInNamespaceNR(SchemeClassSurrogate,
						      gNamespaceName(SchemeClassSurrogate, (object)fp, ns), 
						      cmd);
			} else if (JavaScriptClassSurrogate  &&  IsObj((object)fp)  &&  ClassOf(fp) == JavaScriptString) {
				char	cmd[128];
				sprintf(cmd, "%s(StringToObject(\"%ld\"), %ld)", gStringValue((object)fp), (long) wind, (long) id);
				gExecuteStringNR(JavaScriptClassSurrogate, cmd);
			} else
				fp(wind, id);
		}
	}
	return id;
}






