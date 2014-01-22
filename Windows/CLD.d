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




#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif


#include <windows.h>
#include <string.h>

#include "hdlcache.h"
#include "ctlsave.h"

#ifdef	_WIN32
#include <pshpack2.h>
#include <poppack.h>
#endif


defclass  CLD : Stream  {
	object      iClassName;        // Window Class information

	object      iControlDict;      // String Dictionary of controls - 
	                               // used to drop data to the controls in the dictionary

	object      iControls;         // Linked List of controls 


	object      iMFName;           // metafile name object
	double      iMFScale;          // metafile scaling factor
	
	int         iViewLogPixelsX;   // LOGPIXELSX of the window when the cld was saved
	int         iViewLogPixelsY;   // LOGPIXELSY of the window when the cld was saved

	int         iPage;             // the current page the control is located during printing 

	int         iPageTopMargin;    // the top margin of the page printed
	int         iPageBottomMargin; // the bottom margin of the page printed

init:	class_init;
};


struct CLDObjectNode {
	object obj;
	struct CLDObjectNode * left;
	struct CLDObjectNode * right;
};

typedef struct CLDObjectNode * CLDObjectLink;
static int     addCLDObjectLink(CLDObjectLink rootLink, CLDObjectLink nodeLink);
static int     fillLinkObject(object linkObject, CLDObjectLink rootLink);
static int     destroyCLDObjectLink(CLDObjectLink rootLink);
static object  orderControlsForPrinting(ivType *iv);


static	char	CLD_CODE[] = "CLD";

static	void	class_init()
{
	gDontCollect(CLASS);
}


///////////////////////////////////////////////////////////
// gNewCLD:     construct the CLD printer object
// strCldFile:  the file name saved by the cld
// strMetafile: the metafile name
// 
// Note: if the metafile name is given by strMetafile,
//       the metafile attached in the cld file is ignored;
//       if the metafile is not given by strMetafile and
//       there is a metafile attached in the cld file, the
//       attached metafile will be applied.
//
///////////////////////////////////////////////////////////
cmeth	gNewCLD(char * strCldFile, char * strMetafile)		
{
	CLD_HEADER_t head;
	FILE	     *fp;
	short	     type, size;
	char	     code[sizeof CLD_CODE];
	object	     ctl;

	object	objCld = NULL;
	ivType	*iv = NULL;

	// at least one of the cld file:strCldFile and the metafile:strMetafile should be valid
	if( ( !strCldFile || !(*strCldFile) ) && ( !strMetafile || !(*strMetafile) ) )
		return NULL;

	objCld = gNew(super);
	iv = ivPtr(objCld);

	// default constructor
	iClassName = gNewWithStr(String, "CLD");
	iControlDict = NULL;
	iControls = NULL;
	iMFName = NULL;

	iPage = 0;
	iPageTopMargin = 5;
	iPageBottomMargin = 5;

	if( strMetafile && (*strMetafile) ) 
		iMFName = gNewWithStr(String, strMetafile);

	if(!strCldFile || !(*strCldFile))
		return objCld;  // the cld file is not valid but the metafile is valid

	// load the valid cld file 
	fp = fopen(strCldFile, "rb");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(strCldFile, "rb");
	}

	if (!fp)
		return objCld;

	if (1 != fread(code, sizeof code, 1, fp)  ||
	    1 != fread(&size, sizeof size, 1, fp) ||
	    strcmp(code, CLD_CODE)) {
		fclose(fp);
		return objCld;
	}

	memset(&head, 0, sizeof head);
	if (size == 1) {  // old format - size was version number and there was no header
		head.version = 1;
		head.cxInPixel = 800;
		head.cyInPixel = 600;
		head.logPixelsx = 96;    // default to Small Fonts
		head.logPixelsy = 96;    // default to Small Fonts
		head.metaFileScale = 1.0;
		head.metaFileNameLen = 0;
	} else {
		if (1 != fread(&head, (int)min(size, sizeof(head)), 1, fp)) {
			fclose(fp);
			return NULL; 
		}
		if (size > sizeof(head)) 
			fseek(fp, (long)(size-sizeof(head)), SEEK_CUR); 
		else if(size < sizeof(head)) {
			head.logPixelsx = 96;    // default to Small Fonts 
			head.logPixelsy = 96;    // default to Small Fonts
		}
	}

	iViewLogPixelsX = head.logPixelsx;
	iViewLogPixelsY = head.logPixelsy;
	if( (iViewLogPixelsX<=0) || (iViewLogPixelsY<=0) ) {
		iViewLogPixelsX = 96;
		iViewLogPixelsY = 96;
	}

	iMFScale = head.metaFileScale;
	if(iMFScale <=0) 
		iMFScale = 1.0;

	if (head.metaFileNameLen) {
		char*   tmpMFName; 
		tmpMFName = (char *)malloc(head.metaFileNameLen);
		if (tmpMFName) {
			fread(tmpMFName,  (int) (head.metaFileNameLen), 1, fp);

			// if the metafile name is given by strMetafile,
			// the metafile attached in the cld file is ignored;
			// if the metafile is not given by strMetafile,
			// the metafile attached in the cld file is applied.
			if (!iMFName)
				iMFName = gNewWithStr(String, tmpMFName);

			free(tmpMFName);
		} else {
			fclose(fp);
			return objCld;
		}
	}
	
	while (1 == fread(&type, sizeof type, 1, fp)) {
		switch (type) {
			case CTLTYPE_STATIC:
				ctl = gCLDLoadControl(StaticTextControl, fp, objCld);
				gCLDAddControl(objCld, ctl);
				break;
			case CTLTYPE_TEXT:
				ctl = gCLDLoadControl(TextControl, fp, objCld);
				gCLDAddControl(objCld, ctl);
				break;
			case CTLTYPE_NUMERIC:
				ctl = gCLDLoadControl(NumericControl, fp, objCld);
				gCLDAddControl(objCld, ctl);
				break;
			case CTLTYPE_DATE:
				ctl = gCLDLoadControl(DateControl, fp, objCld);
				gCLDAddControl(objCld, ctl);
				break;
			case CTLTYPE_TIME:
				ctl = gCLDLoadControl(TimeControl, fp, objCld);
				gCLDAddControl(objCld, ctl);
				break;
			case CTLTYPE_RECT:
				ctl = gCLDLoadControl(RectControl, fp, objCld);
				gCLDAddControl(objCld, ctl);
				break;
			case CTLTYPE_LINE:
				ctl = gCLDLoadControl(LineControl, fp, objCld);
				gCLDAddControl(objCld, ctl);
				break;

			// Note: PushButton, RadioButton, CheckBox, ListBox, and ComboBox
			//       are not added to the linklist. Therefore, the
			//       data in those controls are not going to be printed.
			//
			case CTLTYPE_PUSHBUTTON:
				ctl = gCLDLoadControl(PushButton, fp, objCld);
				break;
			case CTLTYPE_RADIOBUTTON:
				gCLDLoadControl(RadioButton, fp, objCld);
				break;
			case CTLTYPE_CHECKBOX:
				gCLDLoadControl(CheckBox, fp, objCld);  // dummy reading
				break;
			case CTLTYPE_LISTBOX:
				gCLDLoadControl(ListBox, fp, objCld);
				break;
			case CTLTYPE_COMBOBOX:
				gCLDLoadControl(ComboBox, fp, objCld);
				break;

			default:
				vError(objCld, "Bad resource file %s", strCldFile);
				break;
		}
	}

	fclose(fp);
	return objCld;
}


////////////////////////////////////////////////////////////////////////
// gCLDAddControl: add the loaded control to the linklist for printing
//
////////////////////////////////////////////////////////////////////////
imeth	gCLDAddControl(ctl)
{
	char	buf[256];

	if (!iControls)
		iControls = gNew(LinkObject);

	gAddLast(iControls, ctl);

	if (!iControlDict)
		iControlDict = gNew(StringDictionary);

	strcpy(buf, gName(ctl));
	if (strlen(buf)==0)
		sprintf(buf, "%d", gSize(iControls));

	gAddStr(iControlDict, buf, ctl);

	return self;
}


///////////////////////////////////////////////////////////////////////
// gCLDGetCtl: return a control object corresponding to the key string 
//
///////////////////////////////////////////////////////////////////////
imeth	gCLDGetCtl(char *strKey)
{
	if(!strKey || !(*strKey))
		return NULL;

	return gFindValueStr(iControlDict, strKey);
}


///////////////////////////////////////////////////
// gCLDSetString:
//
///////////////////////////////////////////////////
imeth	gCLDSetString(char *strKey, char *strValue)
{
	object ctl = NULL;
	if(!strKey || !(*strKey))
		return NULL;

	ctl = gFindValueStr(iControlDict, strKey);
	if(ctl)
		gSetStringValue(ctl, strValue);

	return ctl;
}


////////////////////////////////////////////////////
// gCLDSetDouble:
//
////////////////////////////////////////////////////
imeth	gCLDSetDouble(char *strKey, double dblValue)
{
	object ctl = NULL;
	if(!strKey || !(*strKey))
		return NULL;

	ctl = gFindValueStr(iControlDict, strKey);
	if(ctl)
		gSetDoubleValue(ctl, dblValue);

	return ctl;
}


/////////////////////////////////////////////////
// gCLDSetLong:
//
/////////////////////////////////////////////////
imeth	gCLDSetLong(char *strKey, long longValue)
{
	object ctl = NULL;
	if(!strKey || !(*strKey))
		return NULL;

	ctl = gFindValueStr(iControlDict, strKey);
	if(ctl)
		gSetLongValue(ctl, longValue);

	return ctl;
}


//////////////////////////////////////////////////
// gCLDSetShort:
//
//////////////////////////////////////////////////
imeth	gCLDSetShort(char *strKey, int shortValue)
{
	object ctl = NULL;
	if(!strKey || !(*strKey))
		return NULL;

	ctl = gFindValueStr(iControlDict, strKey);
	if(ctl)
		gSetShortValue(ctl, shortValue);

	return ctl;
}


///////////////////////////////////////////////////////////
// gCLDSetUShort:
//
///////////////////////////////////////////////////////////
imeth	gCLDSetUShort(char *strKey, unsigned unsignedValue)
{
	object ctl = NULL;
	if(!strKey || !(*strKey))
		return NULL;

	ctl = gFindValueStr(iControlDict, strKey);
	if(ctl)
		gSetUShortValue(ctl, unsignedValue);

	return ctl;
}


////////////////////////////////////////////////////////////
// gPrintCLD:  print the cld file and/or the meta file  
// printerObj: the printer object returned by gNewPrinter()
//
////////////////////////////////////////////////////////////
imeth	gPrintCLD(object printerObj)
{
	HDC     hdcPrinter;
	char    *MFName=NULL;

	if(!printerObj)
		return NULL;

	if(iMFName)
		MFName = gStringValue(iMFName);

	if( !( hdcPrinter=gHandle(printerObj) ) ) 
		return NULL;

	// order the controls by their rect.bottom, so multiple
	// page printing can also be processed
	orderControlsForPrinting(iv);

	if( gCLDStartPage(printerObj) ) { 
		gCLDPrintMetaFile(printerObj, MFName); // print the meta file
		if( iControls ) {
			object  seq, ctl;
			int     printerLogPixelsX, printerLogPixelsY;
			double  dScaleX, dScaleY;

			UINT    oldTextAlign;
			int     oldMapMode;

			printerLogPixelsX  = GetDeviceCaps(hdcPrinter, LOGPIXELSX);
			printerLogPixelsY  = GetDeviceCaps(hdcPrinter, LOGPIXELSY);

			if( (iViewLogPixelsX<=0) || (iViewLogPixelsY<=0) ) {
				iViewLogPixelsX = 96;  // small fonts
				iViewLogPixelsY = 96;
			}

			dScaleX = printerLogPixelsX/(double)iViewLogPixelsX;
			dScaleY = printerLogPixelsY/(double)iViewLogPixelsY;

			// use correct text alignment and mapping mode
			oldTextAlign = SetTextAlign(hdcPrinter, TA_LEFT | TA_TOP);
			oldMapMode = SetMapMode(hdcPrinter, MM_TEXT);

			// print the text of the controls in the linklist
			for (seq=gSequence(iControls) ; ctl = gNext(seq) ; ) {
				gPrintCtlScreen(ctl, printerObj, dScaleX, dScaleY, 0, 0);
			}

			// get the old text alignment and mapping mode back
			SetTextAlign(hdcPrinter, oldTextAlign);
			SetMapMode(hdcPrinter, oldMapMode);
		}
		gCLDEndPage(printerObj);
	}
	return self;
}



imeth	gDispose, gDeepDispose ()
{
	// dispose iControls but not the objects in it
	if (iControls)
		iControls = gDispose(iControls);

	// dispose iControlDict and the objects in it
	if (iControlDict) 
		iControlDict = gDeepDispose(iControlDict);

	if(iClassName)
		iClassName = gDispose(iClassName);
	if(iMFName)
		iMFName = gDispose(iMFName);
	return gDispose(super);
}


imeth	double gGetMFScale(object self)
{
	return iMFScale;
}


static int addCLDObjectLink(CLDObjectLink rootLink, CLDObjectLink nodeLink)
{
	RECT rectRoot, rectNode;
	if(!rootLink || !nodeLink)
		return -1;

	gGetWindowRect(rootLink->obj, &rectRoot);
	gGetWindowRect(nodeLink->obj, &rectNode);

	if(rectNode.bottom <= rectRoot.bottom) {
		if(rootLink->left)
			addCLDObjectLink(rootLink->left, nodeLink);
		else 
			rootLink->left = nodeLink;
	}
	else {
		if(rootLink->right)
			addCLDObjectLink(rootLink->right, nodeLink);
		else 
			rootLink->right = nodeLink;
	}
	return 0;
}


static int fillLinkObject(object linkObject, CLDObjectLink rootLink)
{
	if(!linkObject || !rootLink)
		return -1;

	fillLinkObject(linkObject, rootLink->left);
	gAddLast(linkObject, rootLink->obj);
	fillLinkObject(linkObject, rootLink->right);
	return 0;
}


static int destroyCLDObjectLink(CLDObjectLink rootLink)
{
	if(!rootLink)
		return -1;

	destroyCLDObjectLink(rootLink->left);
	destroyCLDObjectLink(rootLink->right);
	
	free(rootLink);
	return 0;
}


///////////////////////////////////////////////////////////////////////////////////////////////////////
// orderControlsForPrinting: put the controls in the linkObject iControls in order by their rect.bottom
//
////////////////////////////////////////////////////////////////////////////////////////////////////////
static object orderControlsForPrinting(ivType *iv)
{ 
	object linkSeq=NULL, link=NULL;
	CLDObjectLink rootLink, nodeLink;

	if (!iControls)
		return NULL;

	// construct rootNode
	linkSeq = gSequenceLinks(iControls); 
	link = gNext(linkSeq);
	if(!link)
		return NULL;

	rootLink = (CLDObjectLink)malloc(sizeof(struct CLDObjectNode));
	rootLink->obj   = gValue(link);
	rootLink->left  = NULL;
	rootLink->right = NULL;

	// construct CLDObjectLink
	for ( ; link = gNext(linkSeq) ; ) {
		nodeLink = (CLDObjectLink)malloc(sizeof(struct CLDObjectNode));
		if(!nodeLink) {
			vError(Application, "memory alocation failure in 'malloc(sizeof(CLDObjectNode))' of 'cld.d'");
		}

		nodeLink->obj   = gValue(link);
		nodeLink->left  = NULL;
		nodeLink->right = NULL;

		addCLDObjectLink(rootLink, nodeLink);
	}

	// create a sorted link object
	gDispose(iControls);
	iControls = gNew(LinkObject);
	fillLinkObject(iControls, rootLink);

	// destroy CLDObjectLink
	destroyCLDObjectLink(rootLink);

	return iControls;
}


imeth	int gGetPage()
{
	return iPage;
}


imeth	int gTurnPage()
{
	return (++iPage);
}


imeth gSetPageMargins(int nTopMargin, int nBottomMargin)
{
	iPageTopMargin = nTopMargin;
	iPageBottomMargin = nBottomMargin;
	return self;
}


imeth gGetPageMargins(int *p_nTopMargin, int *p_nBottomMargin)
{
	*p_nTopMargin = iPageTopMargin;
	*p_nBottomMargin = iPageBottomMargin;
	return self;
}


/////////////////////////////////////////////////////////////////////////////
// In order to use the gPrintCtlScreen function for both the screen and cld 
// file printings, the following dummy functions are required.
/////////////////////////////////////////////////////////////////////////////

imeth BOOL gGetScaleFlg()
{
	return FALSE;
}


imeth gUpdateControlVectors(object ctl)
{
	return self;
}


imeth HANDLE	gHandle()
{
	return (HANDLE) 0;
}


imeth gGetScrollPosition(int *y, int *x)
{
	*y = 0;
	*x = 0;
	return self;
}


imeth int gInDialog()
{
	return 0;
}





