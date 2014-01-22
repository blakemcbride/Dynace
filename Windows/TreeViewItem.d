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


defclass TreeViewItem : PropertyList {
	iTreeView;		//  the actual TreeView control
	iParent;		//  parent TreeViewItem node
	HWND	iItem;
	iChildren;
	iTag;
	int	iAutoDisposeTag;
	iText;
	int	iImage;
};

#include <commctrl.h>


private	imeth	init(tvc, parent, int image, int alpha, char *txt)
{
	TV_INSERTSTRUCT	is;

	iText = gNewWithStr(String, txt);
	iImage = image;
	iTreeView = tvc;
	memset(&is, 0, sizeof is);
	is.hParent = (iParent = parent) ? gHandle(parent) : NULL;
	is.hInsertAfter = !parent &&  alpha ? TVI_SORT : TVI_LAST;
	is.item.mask = TVIF_TEXT | TVIF_PARAM | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_STATE;
	is.item.pszText = gStringValue(iText);
	is.item.lParam = (LPARAM) self;
	is.item.iImage = image;
	is.item.iSelectedImage = image;
	iItem = (HWND) TreeView_InsertItem(gHandle(tvc), &is);

	HC_NEW(WINDOW_HANDLE_CACHE, iItem, self);

	if (parent)
		gAdd(parent, self);

	return self;
}

cmeth	gNewTVItem(tvc, parent, int image, int alpha, char *txt)
{
	return init(gNew(super), tvc, parent, image, alpha, txt);
}

imeth	gDispose, gDeepDispose ()
{
	if (iText)
		gDispose(iText);
	if (iChildren)
		iChildren = gDeepDispose(iChildren);
	if (gInDialog(gDialog(iTreeView)))
		TreeView_DeleteItem(gHandle(iTreeView), iItem);

	if (iTag  &&  iAutoDisposeTag)
		gDeepDispose(iTag);
	gReleaseHandle(self);
	gDisposePropertyList(self);
	return gDispose(super);
}

imeth	gRemoveObj(object item)
{
	gRemoveObj(iChildren, item);
	return self;
}

imeth	gDeleteTVItem()
{
	if (iParent)
		gRemoveObj(iParent, self);
	gDispose(self);
	
	return self;
}

imeth	gGCDispose()
{
	gReleaseHandle(self);
	return gDispose(super);
}

imeth	gDisposeChildren()
{
	if (iChildren)
		iChildren = gDeepDispose(iChildren);
	return (object) 0;
}

imeth	gExpand()
{
	gInternalMessage(iTreeView, 1);
	TreeView_Expand(gHandle(iTreeView), iItem, TVE_EXPAND);
	gInternalMessage(iTreeView, -1);
	return self;
}

imeth	gCollapse()
{
	gInternalMessage(iTreeView, 1);
	TreeView_Expand(gHandle(iTreeView), iItem, TVE_COLLAPSE);
	gInternalMessage(iTreeView, -1);
	return self;
}

imeth	gAdd(tvi)
{
	if (!iChildren)
		iChildren = gNew(Set);
	gAdd(iChildren, tvi);
	return self;
}

imeth	gReleaseHandle()
{
	object	seq, obj;

	if (iChildren)
		for (seq=gSequence(iChildren) ; obj=gNext(seq) ; )
			gReleaseHandle(obj);
	if (iItem) {
		HC_DELETE(WINDOW_HANDLE_CACHE, iItem);
		iItem = 0;
	}
	return self;
}	

imeth	gChildren()
{
	return iChildren;
}

imeth	gGetParent()
{
	return iParent;
}

imeth	gControl()
{
	return iTreeView;
}

imeth	HANDLE	gHandle()
{
	return (HANDLE) iItem;
}

imeth	gSetTag(tag)
{
	object	ptag = iTag;
	iTag = tag;
	if (ptag  &&  iAutoDisposeTag)
		return gDeepDispose(ptag);
	else
		return ptag;
}

imeth	gGetTag()
{
	return iTag;
}

imeth	gAutoDisposeTag()
{
	iAutoDisposeTag = 1;
	return self;
}

imeth	int	gSetAutoDisposeTag(int val)
{
	int	r = iAutoDisposeTag;
	iAutoDisposeTag = val;
	return r;
}

imeth	int	gGetAutoDisposeTag()
{
	return iAutoDisposeTag;
}

imeth	int	gIsChildOf(object parent)
{
	object	child;

	if (!iParent  &&  parent)
		return 0;
	if (iParent == parent)
		return 1;
	for (child = gGetParent(iParent) ; child ; child = gGetParent(child))
		if (child == parent)
			return 1;
	return 0;
}

imeth	gSelect()
{
	gInternalMessage(iTreeView, 1);
	TreeView_SelectItem(gHandle(iTreeView), iItem);
	gInternalMessage(iTreeView, -1);
	return self;
}

imeth	gNext()
{
	HWND	nxt = (HWND) TreeView_GetNextVisible(gHandle(iTreeView), iItem);
	return nxt ? gGetObject(HandleCache, WINDOW_HANDLE_CACHE, nxt) : NULL;
}

imeth	gChild()
{
	HWND	child = (HWND) TreeView_GetChild(gHandle(iTreeView), iItem);
	return child ? gGetObject(HandleCache, WINDOW_HANDLE_CACHE, child) : NULL;
}

imeth	gFirst()
{
	HWND	fst = (HWND) TreeView_GetFirstVisible(gHandle(iTreeView));
	return fst ? gGetObject(HandleCache, WINDOW_HANDLE_CACHE, fst) : NULL;
}

imeth	gSetStringValue(char *val)
{
	TVITEM	itemData;
	HWND	hwnd = gHandle(iTreeView);

	gInternalMessage(iTreeView, 1);
	itemData.hItem = (HTREEITEM) iItem;
	itemData.mask = TVIF_TEXT;
	gChangeStrValue(iText, val);
	itemData.pszText = gStringValue(iText);
	TreeView_SetItem(hwnd, &itemData);
	gSelect(self);
	gInternalMessage(iTreeView, -1);
	
	return self;
}
	
imeth	char *gStringValue()
{
	return gStringValue(iText);
}

imeth	int	gGetImageID()
{
	return iImage;
}

imeth	gChangeImage(int image)
{
	TVITEM	s;
	s.mask = TVIF_IMAGE | TVIF_SELECTEDIMAGE;
	s.hItem = (HANDLE) iItem;
	s.iImage = s.iSelectedImage = image;
	TreeView_SetItem(gHandle(iTreeView), &s);
	iImage = image;
	return self;
}

	





