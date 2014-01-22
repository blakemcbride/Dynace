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

 
 
#include "generics.h"
#include "ctlsave.h"

// DragWindow states
#define DW_STATE_NORMAL         0    // Normal state. 
#define DW_STATE_SELECTED       1    // Outline selecting is in progress. 
#define DW_STATE_DRAGGING       2    // Dragging an existing control. 
#define DW_STATE_GROUP_MOVING   3    // Moving a group of controls. 

// Defines for the different drag handles. 
#define DRAG_CENTER             (-1)
#define DRAG_LEFTBOTTOM         0
#define DRAG_BOTTOM             1
#define DRAG_RIGHTBOTTOM        2
#define DRAG_RIGHT              3
#define DRAG_RIGHTTOP           4
#define DRAG_TOP                5
#define DRAG_LEFTTOP            6
#define DRAG_LEFT               7


defclass  DragWindow { 
	HWND    iDragHwnd;          // the handle for the dragging window
	int     iDragState;         // the state of this object
	RECT    iDragRect;          // inflate this rectangle CHANDLESIZE is the rectangle for the dragging window
	
	object  iSelectedCtl;       // the selected control 
	HWND    iSelectedCtlHwnd;   // the handle for the selected control 

	RECT    iGroupMoveDragRect; // if the selected control is not combobox, it is the same as iDragRect
	                            // if the selected control is combobox, it is the rectangle of the edit
								// part of the control

	object  iTrackRectObj;      // the track rect object

class:
	object  cDWs;               // the set of the instances of the DragWindow
	int     cNumOfDWs;          // the number of the instance of the DragWindow
	object  cCurrentDW;         // the selected current DragWindow

	RECT    cRect;              // the minimum rect that all of the selected controls are inside

	int     cHitHandle;         // the hit handle
	POINT   cMouseDownPos;      // the mouse position

	long    (*cUpdateMenuAndToolBarFunction)(object objMainWindow);

	object  cObjMainWindow;     // the object of cld main window 
init: 
};



static object addToDWs(object obj);
static void   repaintCurrentDW();
static HWND   createDragRect(PRECT pDragRect, HWND selectedCtlHwnd);

static LRESULT CALLBACK DragWindowProc (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
static void   dragRect_wm_paint(object objDragWindow);
static long   dragRect_wm_keydown(object objDragWindow, UINT uMsg, WPARAM wParam, LPARAM lParam);
static long   singleDragRect_wm_lbuttondown(object objDragWindow, UINT uMsg, WPARAM wParam, LPARAM lParam);
static long   dragRect_wm_lbuttondown(object objDragWindow, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
static long   dragRect_wm_mousemove(object objDragWindow, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
static long   dragRect_wm_lbuttonup(object objDragWindow, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
static long   dragRect_wm_rbuttondown(object objDragWindow, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

static int    handleHitTest(HWND hwnd,int x,int y);
static long   adjustRect(object obj, PRECT pRect);
static long   cxCyChar(object obj, int * cxChar, int * cyChar, int * frameWidth);
static long   adjustComboRect(object obj, PRECT pRect, int * minHeight);

static long   drawFrameRect(HDC hDC, PRECT prc, DWORD dwRop);
static void   drawOneHandle(HDC hDC, int x, int y, HBRUSH hbrush);
static void   drawOneFrameHandle(HDC hDC, int x, int y, HBRUSH hBrush);
static void   drawHandles(PRECT pRect, HDC hDC, BOOL bFlag);
static void   setFocus2CurrentDW();



///////////////////////////////////////////////////////////////////////////////
// gNewDragWindow:
//
// construct of the instance of DragWindow
// bFlag  ---  TRUE:  replace the current DragWindow with a new DragWindow
//        ---  FALSE: a new DragWindow is generated and added to the set and 
//                    reset the current DragWindow to the new DragWindow 
///////////////////////////////////////////////////////////////////////////////
cmeth gNewDragWindow(object ctlObj, BOOL bFlag) 
{ 
	object   obj;
	ivType	 *iv;

	if(!ctlObj || !gIsKindOf(ctlObj, Control))
		return NULL;
	
	if(bFlag)
		gRmAllOfDWs(DragWindow);

	obj = gNew(super);
	iv = ivPtr(obj);  

	iSelectedCtl = ctlObj;
	iSelectedCtlHwnd = 	gHandle(ctlObj);
	if(!iSelectedCtlHwnd) {
		obj = gDispose(obj);
		return NULL;
	}
	iDragState = DW_STATE_SELECTED;
	
	if(GetCapture())
		ReleaseCapture();

	SetFocus(GetParent(iSelectedCtlHwnd));
	
	if( ClassOf(iSelectedCtl)==ComboBox ) {
		if(SendMessage(iSelectedCtlHwnd, CB_GETDROPPEDSTATE, (WPARAM)0, 0L)) 
			SendMessage(iSelectedCtlHwnd, CB_SHOWDROPDOWN, (WPARAM)FALSE, 0L); 
	} 

	gGetUpdatedDragRect(obj, &iDragRect);
	gGetUpdatedGroupMoveDragRect(obj, &iGroupMoveDragRect);

	iDragHwnd = createDragRect(&iDragRect, iSelectedCtlHwnd); // initializing iDragHwnd
		
	if(iDragHwnd) {
		object   seq, objTmp;
		HWND     hwndTmp;
		RECT     rectTmp, rectTmp1, rectTmp2;
		COLORREF ctlColor, winColor;
		
		if( ClassOf(iSelectedCtl)==ComboBox )
			EnableWindow(iSelectedCtlHwnd, FALSE);

		ctlColor = gColor(gGetBackBrush(iSelectedCtl));

		if( ClassOf(iSelectedCtl)==CheckBox || ClassOf(iSelectedCtl)==RadioButton
			                                || ClassOf(iSelectedCtl)==StaticTextControl) {
			winColor = gColor(gGetBackBrush(gDialog(iSelectedCtl)));
			
			if (winColor == ctlColor) {
				// if the brush allocation fails, it will not change the back brush
				object objTmp;
				if (winColor == RGB(255, 255, 255)) {
					objTmp = vNew(SolidBrush, 192, 192, 192);
					if(objTmp)
						gBackBrush(iSelectedCtl, objTmp);
				}
				else {
					objTmp = vNew(SolidBrush, 255, 255, 255);
					if(objTmp)
						gBackBrush(iSelectedCtl, objTmp);
				}
			}
			ctlColor = gColor(gGetBackBrush(iSelectedCtl));
		}

		gSetHoldColor(iSelectedCtl, ctlColor); 
		InvalidateRect(iSelectedCtlHwnd, NULL, TRUE);
		// UpdateWindow(iSelectedCtlHwnd);

		// paint the minimum rectangle:
		// get the current DragWindow rectangle
		SetRectEmpty(&rectTmp1);
		if(cCurrentDW) {
			gGetDragRect(cCurrentDW, &rectTmp1);
			InflateRect(&rectTmp1, CHANDLESIZE, CHANDLESIZE);
		}

		addToDWs(obj);  // cCurrentDW is updated inside

		// get the current DragWindow rectangle
		gGetDragRect(cCurrentDW, &rectTmp2);
		InflateRect(&rectTmp2, CHANDLESIZE, CHANDLESIZE);

		// the minimum rectangle needs to be painted.
		UnionRect(&rectTmp, &rectTmp1, &rectTmp2);

		InvalidateRect(hwndTmp=GetParent(iDragHwnd), &rectTmp, FALSE);  // the current DrawWindow will be updated visually
		UpdateWindow (hwndTmp);
	
		if(cDWs && (cNumOfDWs>1) ) { // for ComboBox, when cNumOfDWs>1, adjust the DragWindow's rectangle
			for(seq=gSequence(cDWs); objTmp=gNext(seq);) {
				if( ClassOf(gGetSelectedCtl(objTmp))==ComboBox ) {
					gGetGroupMoveDragRect(objTmp, &rectTmp);
					InflateRect(&rectTmp, CHANDLESIZE, CHANDLESIZE);
					SetWindowPos(gHandle(objTmp), HWND_TOP, rectTmp.left, rectTmp.top,
							rectTmp.right-rectTmp.left, rectTmp.bottom-rectTmp.top, SWP_NOMOVE);
				}
			}
		}
		if(cCurrentDW)
			SetFocus(gHandle(cCurrentDW));

		if( (objTmp=gGetDragWindow(iSelectedCtl)) && objTmp!=obj)
			gDispose(objTmp);

		gSetDragWindow(iSelectedCtl, obj);
	}
	else
		obj = gDispose(obj);

	return obj;
} 



cmeth object gGetCurrentDW()
{
	return cCurrentDW;
}


cmeth object gGetCRect(RECT *pRect)
{
	if(pRect)
		CopyRect(pRect, &cRect);

	return cDWs;
}


cmeth object gGetDWs()
{
	return cDWs;
}


cmeth int gGetNumOfDWs()
{
	return cNumOfDWs;
}


cmeth gGetMainWindowObj()
{
	return cObjMainWindow;
}


cmeth gSetMainWindowObj(object obj)
{
	cObjMainWindow = obj;
	return NULL;
}


cmeth  long gDeleteDragWindows()
{
	object parentObj, selectedCtl, seq, objTmp;

	if(!cDWs)
		return -1L;

	for(seq=gSequence(cDWs); objTmp=gNext(seq);) {
		selectedCtl = gGetSelectedCtl(objTmp);
		parentObj = gGetParent(selectedCtl);

		gSetModifyChildren(parentObj, 0);
		gHide(selectedCtl);
		gSetModifyChildren(parentObj, 1);

		gSetTabOrder(selectedCtl, -1);		       //  mark as deleted
		gAddToDeletedCtls(parentObj, selectedCtl); // keep track of the deleted controls

		gDispose(objTmp);
	}

	return 0L;
}



// remove every obj from the set of dragwindows
cmeth gRmAllOfDWs()
{
	object seq, objTmp;
	if(!cDWs)
		return NULL;

	for(seq=gSequence(cDWs); objTmp=gNext(seq);) 
			gDispose(objTmp);
	
    return NULL;
}


///////////////////////////////////////////////////
// recalculate the unionized rectangle in the set
//
cmeth void gRecalculateCRect()
{
	object seq,   objTmp;
	RECT   rect1, rect2;

	if((!cDWs) || (!cCurrentDW) || (cNumOfDWs<1))
		return;

	// only one Dragwindow
	if(cNumOfDWs==1) {
		gGetDragRect(cCurrentDW, &cRect);  
		return;
	}

	SetRectEmpty(&cRect);
	for(seq=gSequence(cDWs); objTmp=gNext(seq);) {
		CopyRect(&rect1, &cRect);
		gGetGroupMoveDragRect(objTmp, &rect2);  
		UnionRect(&cRect, &rect1, &rect2);
	}
}



cmeth ofun gSetUpdateMenuAndToolBarFunction(long (*fun)(object))
{
	ofun org = (ofun) cUpdateMenuAndToolBarFunction;
	if (IsObj((object) cUpdateMenuAndToolBarFunction)) {
		gDispose((object) cUpdateMenuAndToolBarFunction);
		org = NULL;
	}
	cUpdateMenuAndToolBarFunction = fun;
	return org;
}



/////////////////////////////////////////////////////////////////////////////////////////////
// dragRect__wm_keydown:
// 
// keyboard functions
//
/////////////////////////////////////////////////////////////////////////////////////////////
static long dragRect_wm_keydown(object objDragWindow, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	HDWP    hdwp;
	RECT    rcDrag;
	int     dx, dy;
	object  seq, objTmp;

	if(!objDragWindow || !cDWs || cNumOfDWs<1)
		return -1L;

	dx = 0;
	dy = 0;		
	switch(wParam) {
		case VK_DELETE:
			gDeleteDragWindows(DragWindow);
			return 0L;

		case VK_UP:
			dy = -1;
			break;

		case VK_DOWN:
			dy = 1;
			break;

		case VK_LEFT:
			dx = -1;
			break;

		case VK_RIGHT:
			dx = 1;
			break;

		default:
			return 0L;
	}

	for(seq=gSequence(cDWs); objTmp=gNext(seq);) {
		gGetUpdatedDragRect(objTmp, &rcDrag); 
		gGetUpdatedGroupMoveDragRect(objTmp, &rcDrag);

		if(dx < 0)
			dx = max(CHANDLESIZE - rcDrag.left, dx);
		if(dy < 0)
			dy = max(CHANDLESIZE - rcDrag.top, dy);

		// GetClientRect(hPwnd, &rectP);
		// if(dx > 0)
		//	dx = min(rcDrag.right-CHANDLESIZE-rcDrag.right, dx);
		// if(dy > 0)
		//	dy = min(rcDrag.bottom-CHANDLESIZE-rcDrag.bottom, dy);
	}

	if(dx==0 && dy==0)
		return 0L;

	for(seq=gSequence(cDWs); objTmp=gNext(seq);) {
 		gMoveDragRect(objTmp, dx, dy);           // update iDragRect
		gMoveGroupMoveDragRect(objTmp, dx, dy);  // update iGroupMoveDragRect
		
		// update the DragWindow visually
		gGetDragRect(objTmp, &rcDrag);
		gSetWindowPositions(objTmp, &rcDrag);
	}

	// recalculate the unionized rectangle in the set
	gRecalculateCRect(DragWindow);

	if( cObjMainWindow && gIsKindOf(cObjMainWindow, Window) )
		gUpdateScrollBar(cObjMainWindow);

    return 0L;
}


//////////////////////////////////////////////
// addToDWs;
// add the obj to the set of the DragWindows
//
static  object addToDWs(object obj)
{
	if (obj)  {   // attach to the link list
		if (!cDWs) 
			cDWs = gNew(Set);
	
		gAdd(cDWs, obj);

		cNumOfDWs++;      // the number of controls in the link list increase 1
		cCurrentDW = obj;
	
		// update the cRect rectangle
		gRecalculateCRect(DragWindow);
		SetFocus(gHandle(cCurrentDW));

		if(cUpdateMenuAndToolBarFunction)
			(*cUpdateMenuAndToolBarFunction)(cObjMainWindow);  // update toolbar appearance
	}
    return cDWs;
}



////////////////////////////////////////////////////////////////////
// createDragRect:
//
// PRECT pDragRect: a point to the rectandle
// Hwnd selectedCtlHwnd: the window handle of the selected control
////////////////////////////////////////////////////////////////////
static HWND createDragRect(PRECT pDragRect, HWND selectedCtlHwnd)
{
	HDWP        hdwp;
	HWND        hwnd, hPwnd;
	RECT        rcDrag;
	WNDCLASS    wndclass;
	HINSTANCE   hinstance;

	if(!pDragRect || !selectedCtlHwnd) 
		return (HWND)0;

	hPwnd = GetParent(selectedCtlHwnd);
	hinstance = (HINSTANCE) gInstance(Application);

	// wndclass.cbSize     = sizeof (wndclass) ;
	wndclass.style         = 0; 
	wndclass.lpfnWndProc   = DragWindowProc ;
	wndclass.cbClsExtra    = 0 ;
	wndclass.cbWndExtra    = 0 ;
	wndclass.hInstance     = hinstance;
	wndclass.hIcon         = (HICON)0 ;
	wndclass.hCursor       = LoadCursor ((HINSTANCE)0, IDC_ARROW) ;
	wndclass.hbrBackground = (HBRUSH) (GetStockObject(NULL_BRUSH));
	wndclass.lpszMenuName  = NULL ;
	wndclass.lpszClassName = "DragWindow";
	// wndclass.hIconSm    = NULL;
 
	RegisterClass(&wndclass) ;

	hwnd = CreateWindow ("DragWindow", NULL, WS_CHILD, 0,0,0,0, hPwnd, 0, hinstance, 0) ;
 	
	CopyRect(&rcDrag, pDragRect);

	hdwp = BeginDeferWindowPos(2);
	hdwp = DeferWindowPos(hdwp, selectedCtlHwnd, hwnd, rcDrag.left, rcDrag.top,
				(rcDrag.right-rcDrag.left), (rcDrag.bottom-rcDrag.top), SWP_NOACTIVATE );
	InflateRect(&rcDrag, CHANDLESIZE, CHANDLESIZE);
	hdwp = DeferWindowPos(hdwp, hwnd, HWND_TOP, rcDrag.left, rcDrag.top,
		(rcDrag.right-rcDrag.left), (rcDrag.bottom-rcDrag.top),	SWP_SHOWWINDOW);
	EndDeferWindowPos(hdwp);

	return hwnd;
}
  


//////////////////
// DragRectProc
//
static LRESULT CALLBACK DragWindowProc (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	object  seq, obj, objDragWindow=NULL;
	RECT    rect;

	if(uMsg == WM_NCCREATE)
		return DefWindowProc (hwnd, uMsg, wParam, lParam);

	// from the handle, get the corresponding DragWindow object.
	if(cDWs)
		for(seq=gSequence(cDWs); obj=gNext(seq);) {
			if(gHandle(obj) == hwnd) {  // obj is instance of this class
				objDragWindow= obj;
				gDispose(seq);
				break;
			}
		}
	
	if(!objDragWindow)
		return DefWindowProc (hwnd, uMsg, wParam, lParam);
		
	switch (uMsg)
	{
		case WM_PAINT:
			dragRect_wm_paint(objDragWindow);
			setFocus2CurrentDW();
			break;

		case WM_LBUTTONDOWN:
			dragRect_wm_lbuttondown(objDragWindow, hwnd, uMsg, wParam, lParam);
			return 0L;

		case WM_MOUSEMOVE:
			dragRect_wm_mousemove(objDragWindow, hwnd, uMsg, wParam, lParam);
			setFocus2CurrentDW();
			return 0L;

		case WM_LBUTTONUP:
			dragRect_wm_lbuttonup(objDragWindow, hwnd, uMsg, wParam, lParam); 
			return 0L;

		case WM_KEYDOWN:
			dragRect_wm_keydown(objDragWindow, uMsg, wParam, lParam);
			break;

		case WM_RBUTTONDOWN:
			dragRect_wm_rbuttondown(objDragWindow, hwnd, uMsg, wParam, lParam);
			return 0L;

	} // switch (uMsg)

	return DefWindowProc (hwnd, uMsg, wParam, lParam);
}
 

static void setFocus2CurrentDW() 
{
	if(cCurrentDW) {
		HWND hwndCurrentDW = gHandle(cCurrentDW); 
		if( GetFocus() != hwndCurrentDW)
			SetFocus(hwndCurrentDW);
	}
}



// dragRect_wm_paint
static void dragRect_wm_paint(object objDragWindow)
{
	HDC          hDC;
	PAINTSTRUCT  ps;
	RECT         rcDrag;
	HWND         hwnd;
	
	if(!objDragWindow)
		return;

	hwnd=gHandle(objDragWindow);

	if( cNumOfDWs<=1 ) {  // if there is only one combobox selected, show the droppeddownlist
		gGetDragRect(objDragWindow, &rcDrag);
		InflateRect(&rcDrag, CHANDLESIZE, CHANDLESIZE);

		hDC = BeginPaint(hwnd, &ps);

		// draw the frame rect
		drawFrameRect(hDC, &rcDrag, PATCOPY);

		// draw the handles
		if(objDragWindow == cCurrentDW)
			drawHandles(&rcDrag, hDC, TRUE);    // solid handles
		else
			drawHandles(&rcDrag, hDC, FALSE);   // frame handles

		EndPaint(hwnd, &ps);
	}
	else {    // cNumOfDWs>1    only the edit control part of the combobox is shown
		gGetGroupMoveDragRect(objDragWindow, &rcDrag);
		InflateRect(&rcDrag, CHANDLESIZE, CHANDLESIZE);
		
		hDC = BeginPaint(hwnd, &ps);
 
		// draw the frame rect
		drawFrameRect(hDC, &rcDrag, PATCOPY);

		// draw the handles
		if(objDragWindow == cCurrentDW)
			drawHandles(&rcDrag, hDC, TRUE);    // solid handles
		else
			drawHandles(&rcDrag, hDC, FALSE);   // frame handles

		EndPaint(hwnd, &ps);
	}
}



/////////////////////////////////////
// singleDragRect_wm_lbuttondown
//
static long singleDragRect_wm_lbuttondown(object objDragWindow, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	// Note: hwnd = dragHwnd, where hwnd is the handle associated with the message,
	//       and dragHwnd is the DragWindow handle of object objDragWindow
	
	POINT	 p;
	int      xMid, yMid;
	RECT     rect, dragRect;
	HWND     dragHwnd, hwndP;
	object   trackRectObj;

	if(!objDragWindow)
		return -1L;

	dragHwnd = gHandle(objDragWindow);
	hwndP = GetParent(gGetSelectedCtlHwnd(objDragWindow));

	if(wParam & MK_LBUTTON) {  // the tracking process begins for the current DragWindow
 		SetCapture(dragHwnd);
 		gSetDragState(objDragWindow, DW_STATE_DRAGGING);
		
		gGetUpdatedGroupMoveDragRect(objDragWindow, NULL);
		gGetUpdatedDragRect(objDragWindow, &dragRect);

		trackRectObj = gInitTracking(TrackRect, hwndP);
 		gDrawTrackRect(trackRectObj, &dragRect, TRUE);
		gSetTrackRectObj(objDragWindow, trackRectObj);
		
 		GetCursorPos(&p);              // p is in the screen coordinates
 		ScreenToClient(dragHwnd, &p);  // p is in the client coordinates of this window (iDragHwnd).
		 
		xMid = (dragRect.left + dragRect.right + 1)/2;
 		yMid = (dragRect.top + dragRect.bottom + 1)/2;
 
		cHitHandle = handleHitTest(dragHwnd, p.x, p.y);
 		switch (cHitHandle) {	// do the handle hit test
			case DRAG_LEFTTOP:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENWSE));
				cMouseDownPos.x = dragRect.left;
				cMouseDownPos.y = dragRect.top;
				break;
 
			case DRAG_LEFTBOTTOM:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENESW));
				cMouseDownPos.x = dragRect.left;
				cMouseDownPos.y = dragRect.bottom;
				break;
 
			case DRAG_LEFT:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEWE));
				cMouseDownPos.x = dragRect.left;
				cMouseDownPos.y = yMid;
				break;
 
			case DRAG_RIGHTTOP:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENESW));
				cMouseDownPos.x = dragRect.right;
				cMouseDownPos.y = dragRect.top;
				break;
 
			case DRAG_RIGHTBOTTOM:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENWSE));
				cMouseDownPos.x = dragRect.right;
				cMouseDownPos.y = dragRect.bottom;
				break;
 
			case DRAG_RIGHT:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEWE));
				cMouseDownPos.x = dragRect.right;
				cMouseDownPos.y = yMid;
				break;
 
			case DRAG_TOP:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENS));
				cMouseDownPos.x = xMid;
				cMouseDownPos.y = dragRect.top;
				break;
 
			case DRAG_BOTTOM:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENS));
				cMouseDownPos.x = xMid;
				cMouseDownPos.y = dragRect.bottom;
				break;
 
			case DRAG_CENTER:
 
			default:
#ifdef WIN32
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEALL));
#else
				SetCursor(LoadCursor((HINSTANCE)0,IDC_CROSS));
#endif
				ClientToScreen(dragHwnd, &p);
				ScreenToClient(GetParent(dragHwnd), &p); 

				// p in the client coordinates of dragHwnd's Parent window.
				cMouseDownPos.x = p.x;
				cMouseDownPos.y = p.y;
				break;
 		} // switch (cHitHandle)
 
    } // if(wParam & MK_LBUTTON)
    return 0L;
}



///////////////////////////////
// dragRect_wm_rbuttondown
//
static long   dragRect_wm_rbuttondown(object objDragWindow, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	HWND   selectedCtlHwnd;
	RECT   rcDrag;
	object selectedCtl;

	//  this function functions only when there is only one DragWindow
	if(!objDragWindow || cNumOfDWs!=1 || gHandle(objDragWindow)!=hwnd)  
		return -1L;    

	selectedCtlHwnd = gGetSelectedCtlHwnd(objDragWindow);  
	SendMessage(selectedCtlHwnd, uMsg, wParam, 0L);

	if(GetCapture())
		ReleaseCapture();

	gGetUpdatedGroupMoveDragRect(objDragWindow, NULL);  // update groupMoveDragRect
	gGetUpdatedDragRect(objDragWindow, &rcDrag); 

	selectedCtl = gGetSelectedCtl(objDragWindow);
	gSetWindowRect(selectedCtl, &rcDrag);
	gUpdateControlVectors(gDialog(selectedCtl), selectedCtl);

	// update the DragWindow visually
	InflateRect(&rcDrag, CHANDLESIZE, CHANDLESIZE);
	SetWindowPos(hwnd, (HWND)0, rcDrag.left, rcDrag.top,
	            (rcDrag.right-rcDrag.left), (rcDrag.bottom-rcDrag.top),	SWP_NOZORDER);

	// the following statement should be the last one
	if(	wParam == (MK_CONTROL | MK_RBUTTON) ) 
		gDispose(objDragWindow);

	return 0L;
}



///////////////////////////////
// dragRect_wm_lbuttondown
//
static long dragRect_wm_lbuttondown(object objDragWindow, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	// Note: hwnd = dragHwnd, where hwnd is the handle associated with the message,
	//       and dragHwnd is the DragWindow handle of object objDragWindow

	POINT	    p;
	object      dlg, seq, obj;
	HWND        hPwnd;
	RECT        rectDrag, rectTmp1, rectTmp2;

	if(!objDragWindow || !cDWs || !cCurrentDW || cNumOfDWs<1 || gHandle(objDragWindow)!=hwnd)
		return -1L;

	if(cNumOfDWs == 1) { // handle the single DragWindow
		if(cCurrentDW==objDragWindow) 
			if(wParam & MK_SHIFT)  // the control was selected already, the second time selection will deSelects it
				gDispose(objDragWindow);    
			else
				singleDragRect_wm_lbuttondown(objDragWindow, uMsg, wParam, lParam);  
				// mouse is SetCapture inside the routine
	}
	else {  // cNumOfDWs > 1
 		if( wParam & MK_CONTROL )  { // change current Dragwindow
			if(cCurrentDW == objDragWindow)
				return 0L;

			// paint the minimum rectangle:
			// get the current DragWindow rectangle
			SetRectEmpty(&rectTmp1);
			if(cCurrentDW) {
				gGetDragRect(cCurrentDW, &rectTmp1);  
				InflateRect(&rectTmp1, CHANDLESIZE, CHANDLESIZE);
			}

			cCurrentDW = objDragWindow;       // update current Dragwindow

			// get the current DragWindow rectangle
			gGetDragRect(cCurrentDW, &rectTmp2);  // cCurrentDW is instance of this class
			InflateRect(&rectTmp2, CHANDLESIZE, CHANDLESIZE);

			// the minimum rectangle needs to be painted.
			UnionRect(&rectDrag, &rectTmp1, &rectTmp2);

			InvalidateRect(hPwnd=GetParent(hwnd), &rectDrag, FALSE);  // the current DrawWindow will be updated visually
			UpdateWindow (hPwnd);

			if(cUpdateMenuAndToolBarFunction)
				(*cUpdateMenuAndToolBarFunction)(cObjMainWindow);  // update toolbar appearance
		}
		else if(wParam & MK_SHIFT) 
			gDispose(objDragWindow); 
		else if (GetAsyncKeyState(VK_MENU) & 0x8000 ? TRUE : FALSE) {  // Alt key pressed
			for(seq=gSequence(cDWs); obj=gNext(seq);) {    // except the current one
				if(obj != objDragWindow)  
					gDispose(obj);
			}
		}
		else {    // move grouped controls
			object trackRectObj;
			SetCapture(hwnd);
			hPwnd = GetParent(hwnd);    // hPwnd is the handle of the parent window of the controls
			for(seq=gSequence(cDWs); obj=gNext(seq);) {
 				gSetDragState(obj, DW_STATE_GROUP_MOVING);  
				gGetUpdatedDragRect(obj, NULL);
				gGetUpdatedGroupMoveDragRect(obj, &rectTmp2);

				CopyRect(&rectDrag, &rectTmp2);
				trackRectObj = gInitTracking(TrackRect, hPwnd);
 				gDrawTrackRect(trackRectObj, &rectDrag, TRUE);
				gSetTrackRectObj(obj, trackRectObj);
			}

 			GetCursorPos(&p);             // p is in the screen coordinates
 			ScreenToClient(hPwnd, &p);    // p is in the client coordinates of the Parent window.
#ifdef WIN32
			SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEALL));
#else
			SetCursor(LoadCursor((HINSTANCE)0,IDC_CROSS));
#endif
			// p in the client coordinates of the Parent window.
			cMouseDownPos.x = p.x;
			cMouseDownPos.y = p.y;
			cHitHandle = DRAG_CENTER;
		}
	}

    return 0L;
}



////////////////////////////
// dragRect_wm_mousemove
//
static long dragRect_wm_mousemove(object objDragWindow, HWND hwnd, UINT	uMsg, WPARAM wParam, LPARAM	lParam)
{
 	POINT	p;
	HWND    hPwnd;
	RECT    rectP;
	int     cxChar, cyChar, frameWidth;
	object  seq, obj, trackRectObj;
	int     localHitHandle, dx=0, dy=0;

	if(!objDragWindow || !cDWs || !cCurrentDW || cNumOfDWs<1 || gHandle(objDragWindow)!=hwnd)
		return -1L;
 
	GetCursorPos(&p);     // p is in the screen coordinates
	hPwnd = GetParent(hwnd);

	// the handle senses the mouse position when the mouse is moving on the handle but the 
	// LBUTTON is not down,
 	if(!(wParam & MK_LBUTTON) ) {    
		// grouped controls:
		if(cNumOfDWs>1)  { // (wParam & MK_LBUTTON) && (cNumOfDWs>1), move grouped controls 
#ifdef WIN32
 			SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEALL));
#else
 			SetCursor(LoadCursor((HINSTANCE)0,IDC_CROSS));
#endif
		}
		else {    // single control
			ScreenToClient(hwnd, &p);  // p is in the client coordinates of this window.
			localHitHandle = handleHitTest(hwnd, p.x, p.y);  
			switch (localHitHandle) { // handle hit test
				case DRAG_LEFTBOTTOM:
				case DRAG_RIGHTTOP: 
					SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENESW));
					break;
 				case DRAG_BOTTOM:
				case DRAG_TOP: 
					SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENS));
					break;
				case DRAG_RIGHTBOTTOM:
				case DRAG_LEFTTOP: 
					SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENWSE));
					break;
				case DRAG_RIGHT:
				case DRAG_LEFT: 
					SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEWE));
					break;
				default:
#ifdef WIN32
					SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEALL));
#else
					SetCursor(LoadCursor((HINSTANCE)0,IDC_CROSS));
#endif
					break;
			} // witch (localHitHandle) 
		}
	}
 	else if(cNumOfDWs>1)  { // (wParam & MK_LBUTTON) && (cNumOfDWs>1), move grouped controls 
		RECT rectTmp;
#ifdef WIN32
 		SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEALL));
#else
 		SetCursor(LoadCursor((HINSTANCE)0,IDC_CROSS));
#endif
		ScreenToClient(hPwnd, &p); // p is in the client coordinates of the Parent window.
		dx = p.x - cMouseDownPos.x;
 		dy = p.y - cMouseDownPos.y;
		for(seq=gSequence(cDWs); obj=gNext(seq);) {
			gSetTrackRectObj(obj, NULL);
			gGetGroupMoveDragRect(obj, &rectTmp);

			if(dx < 0)
				dx = max(CHANDLESIZE - rectTmp.left, dx);
			if(dy < 0)
				dy = max(CHANDLESIZE - rectTmp.top, dy);

			// GetClientRect(hPwnd, &rectP);
			// if(dx > 0)
			//	dx = min(rectP.right-CHANDLESIZE-rectTmp.right, dx);
			// if(dy > 0)
			//	dy = min(rectP.bottom-CHANDLESIZE-rectTmp.bottom, dy);
		}

		p.x = dx + cMouseDownPos.x;
		p.y = dy + cMouseDownPos.y;

		for(seq=gSequence(cDWs); obj=gNext(seq);) {
			if(gGetDragState(obj) == DW_STATE_GROUP_MOVING) {
				gMoveGroupMoveDragRect(obj, dx, dy);
				gGetGroupMoveDragRect(obj, &rectTmp);

				trackRectObj = gInitTracking(TrackRect, hPwnd);
 				gDrawTrackRect(trackRectObj, &rectTmp, TRUE);
				gSetTrackRectObj(obj, trackRectObj);
			}
		}

 		cMouseDownPos = p;
	}
	else if(gGetDragState(objDragWindow) == DW_STATE_DRAGGING)  { // (wParam & MK_LBUTTON) && cNumOfDWs=1
		RECT dragRect;
		object selectedCtl;

		gGetDragRect(objDragWindow, &dragRect);
		selectedCtl = gGetSelectedCtl(objDragWindow);
	
		// get cxChar and cyChar
		cxCyChar(selectedCtl, &cxChar, &cyChar, &frameWidth);
		cyChar += frameWidth;

		// for combobox, adjust cxChar and cyChar, where cyChar is the minimum Height of the combobox. 
		if(ClassOf(selectedCtl)==ComboBox) {
			cxChar = 2*cxChar;
			adjustComboRect(selectedCtl, &rectP, &cyChar);  // Note: rectP is only a place hold here.
 		}
		else if(ClassOf(selectedCtl)==LineControl) {
			cxChar = 1;
			cyChar = 1;    
 		}
 
		GetClientRect(hPwnd, &rectP);    // get the rectangle of the parent window's client area
		ScreenToClient(hPwnd, &p);       // p is in the client coordinates of the Parent window.

		switch (cHitHandle) {
			case DRAG_LEFTTOP:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENWSE));
				dx = p.x - cMouseDownPos.x;
				dy = p.y - cMouseDownPos.y;
				dragRect.left += dx;
				dragRect.top += dy;
				if(dx > 0) {
					dragRect.left = min(dragRect.left, dragRect.right-cxChar);
				}
				else if (dx<0) {
					dragRect.left = max(dragRect.left, CHANDLESIZE);	
				}
				if(dy > 0) {
					dragRect.top = min(dragRect.top, dragRect.bottom-cyChar);
				}
				else if (dy<0) {
					dragRect.top = max(dragRect.top, CHANDLESIZE);	
				}
				break;
 
			case DRAG_LEFTBOTTOM:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENESW));
				dx = p.x - cMouseDownPos.x;
				dy = p.y - cMouseDownPos.y;
				dragRect.left += dx;
				dragRect.bottom += dy;
				if(dx > 0) {
				    dragRect.left = min(dragRect.left, dragRect.right-cxChar);
				}
				else if (dx<0) {
 					dragRect.left = max(dragRect.left, CHANDLESIZE);	
 				}
 
			//	if(dy > 0) {
			//		dragRect.bottom = min(dragRect.bottom, rectP.bottom-CHANDLESIZE);
			//	}
			//	else if (dy<0) {

				if (dy<0) {
					dragRect.bottom = max(dragRect.bottom, dragRect.top+cyChar);	
				}
				break;
 
			case DRAG_LEFT:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEWE));
				dx = p.x - cMouseDownPos.x;
				dragRect.left += dx;
				if(dx > 0) {
					dragRect.left = min(dragRect.left, dragRect.right-cxChar);
				}
				else if (dx<0) {
					dragRect.left = max(dragRect.left, CHANDLESIZE);	
				}
				break;
 
			case DRAG_RIGHTTOP:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENESW));
				dx = p.x - cMouseDownPos.x;
				dy = p.y - cMouseDownPos.y;
				dragRect.right += dx;
				dragRect.top += dy;
 
			//	if(dx > 0) {
			//		dragRect.right = min(dragRect.right, rectP.right-CHANDLESIZE);
			//	}
			//	else if (dx<0) {
 
				if (dx<0) {
					dragRect.right = max(dragRect.right, dragRect.left+cxChar);	
				}
				if(dy > 0) {
					dragRect.top = min(dragRect.top, dragRect.bottom-cyChar);
				}
				else if (dy<0) {
					dragRect.top = max(dragRect.top, CHANDLESIZE);	
				}
				break;
 
			case DRAG_RIGHTBOTTOM:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENWSE));
				dx = p.x - cMouseDownPos.x;
				dy = p.y - cMouseDownPos.y;
				dragRect.right += dx;
				dragRect.bottom += dy;
 
			//	if(dx > 0) {
			//		dragRect.right = min(dragRect.right, rectP.right-CHANDLESIZE);
			//	}
			//	else if (dx<0) {
				if (dx<0) {
					dragRect.right = max(dragRect.right, dragRect.left+cxChar);	
				}
 
			//	if(dy > 0) {
			//		dragRect.bottom = min(dragRect.bottom, rectP.bottom-CHANDLESIZE);
			//	}
			//	else if (dy<0) {
 
				if (dy<0) {
					dragRect.bottom = max(dragRect.bottom, dragRect.top+cyChar);	
				}
				break;
 
			case DRAG_RIGHT:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEWE));
				dx = p.x - cMouseDownPos.x;
				dragRect.right += dx;

			//	if(dx > 0) {
			//		dragRect.right = min(dragRect.right, rectP.right-CHANDLESIZE);
			//	}
			//	else if (dx<0) {

				if (dx<0) {
					dragRect.right = max(dragRect.right, dragRect.left+cxChar);	
				}
				break;
 
			case DRAG_TOP:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENS));
				dy = p.y - cMouseDownPos.y;
				dragRect.top += dy;
 
				if(dy > 0) {
					dragRect.top = min(dragRect.top, dragRect.bottom-cyChar);
				}
				else if (dy<0) {
					dragRect.top = max(dragRect.top, CHANDLESIZE);	
				}
				break;
 
			case DRAG_BOTTOM:
				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZENS));
				dy = p.y - cMouseDownPos.y;
				dragRect.bottom += dy;
 
			//	if(dy > 0) {
 			//		dragRect.bottom = min(dragRect.bottom, rectP.bottom-CHANDLESIZE);
 			//	}
 			//	else if (dy<0) {
 
				if (dy<0) {
 					dragRect.bottom = max(dragRect.bottom, dragRect.top+cyChar);	
 				}
 				break;
 
			case DRAG_CENTER:
#ifdef WIN32
 				SetCursor(LoadCursor((HINSTANCE)0,IDC_SIZEALL));
#else
 				SetCursor(LoadCursor((HINSTANCE)0,IDC_CROSS));
#endif
 				dx = p.x - cMouseDownPos.x;
 				dy = p.y - cMouseDownPos.y;
 				dragRect.left += dx;
 				dragRect.right += dx;
 				dragRect.top += dy;
 				dragRect.bottom += dy;

 				if(dragRect.left < CHANDLESIZE) {
 					dragRect.right -= (dragRect.left-CHANDLESIZE);
 					dragRect.left = CHANDLESIZE;
 				}
 
				if(dragRect.top < CHANDLESIZE) {
 					dragRect.bottom -= (dragRect.top-CHANDLESIZE);
 					dragRect.top = CHANDLESIZE;
 				}
 
			//	if(dragRect.right > (rectP.right-CHANDLESIZE)) {
 			//		dragRect.left = dragRect.left - dragRect.right + rectP.right-CHANDLESIZE;
 			//		dragRect.right = rectP.right-CHANDLESIZE;
 			//	}
 
			//	if(dragRect.bottom > (rectP.bottom-CHANDLESIZE)) {
 			//		dragRect.top = dragRect.top - dragRect.bottom + rectP.bottom-CHANDLESIZE;
 			//		dragRect.bottom = rectP.bottom-CHANDLESIZE;
 			//	}
 
				break;
 
			default:
 				break;
 
		} // switch (cHitHandle) 

		gSetDragRect(objDragWindow, &dragRect, NULL);

		trackRectObj = gInitTracking(TrackRect, hPwnd);
 		gDrawTrackRect(trackRectObj, &dragRect, TRUE);
		gSetTrackRectObj(objDragWindow, trackRectObj);
		cMouseDownPos = p;
	}
 
	return 0L;
}
 


/////////////////////////////
// dragRect_wm_lbuttonup
//
static long dragRect_wm_lbuttonup(object objDragWindow, HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
 	HDWP     hdwp;
	RECT     rcDrag, rectTmp;
	int      minHeight;

	HWND     dragHwnd, selectedCtlHwnd;
	object   seq, obj, selectedCtl, parentObj;

	if(GetCapture())
		ReleaseCapture(); 
	
	if(!objDragWindow || !cDWs || !cCurrentDW || cNumOfDWs<1 || gHandle(objDragWindow)!=hwnd)
		return -1L;

	if(cNumOfDWs > 1)  { // move grouped controls 
		for(seq=gSequence(cDWs); obj=gNext(seq);) {
			if(gGetDragState(obj) == DW_STATE_GROUP_MOVING) {
				gSetTrackRectObj(obj, NULL);
 				gSetDragState(obj, DW_STATE_SELECTED);

				dragHwnd = gHandle(obj);
				gGetGroupMoveDragRect(obj, &rcDrag);  // it is ok when it is used with SWP_NOSIZE 
				selectedCtl = gGetSelectedCtl(obj);
				selectedCtlHwnd = gGetSelectedCtlHwnd(obj);

				hdwp = BeginDeferWindowPos(2);
				hdwp = DeferWindowPos(hdwp, selectedCtlHwnd, dragHwnd, rcDrag.left, rcDrag.top,
				                      0, 0, SWP_NOACTIVATE | SWP_NOSIZE);
 				// adjust the drag window
 				InflateRect(&rcDrag, CHANDLESIZE, CHANDLESIZE);
 				hdwp = DeferWindowPos(hdwp, dragHwnd, (HWND)0, rcDrag.left, rcDrag.top,
 				                      0, 0,	SWP_NOZORDER | SWP_NOSIZE);
 				EndDeferWindowPos(hdwp);

				// update dragRect from groupMoveDragRect
				gGetUpdatedDragRect(obj, &rcDrag);   // update dragRect
				gSetWindowRect(selectedCtl, &rcDrag);
				gUpdateControlVectors(gDialog(selectedCtl), selectedCtl);
			}
		}
	}
	else if(gGetDragState(objDragWindow) == DW_STATE_DRAGGING) {    // cNumOfDWs = 1
		RECT dragRect;
		gSetTrackRectObj(objDragWindow, NULL);
		gSetDragState(objDragWindow, DW_STATE_SELECTED);
 
		selectedCtl = gGetSelectedCtl(objDragWindow);
		selectedCtlHwnd = gGetSelectedCtlHwnd(objDragWindow);

		// dragRect is the source for updating
		gGetDragRect(objDragWindow, &dragRect);

		// adjust dragRect
		if(ClassOf(selectedCtl)==ListBox)
			adjustRect(selectedCtl, &dragRect);
		else if(ClassOf(selectedCtl)==ComboBox )
			adjustComboRect(selectedCtl, &dragRect, &minHeight);

		gSetDragRect(objDragWindow, &dragRect, NULL);
		gSetWindowRect(selectedCtl, &dragRect);

		CopyRect(&rcDrag, &dragRect);

		hdwp = BeginDeferWindowPos(2);
		hdwp = DeferWindowPos(hdwp, selectedCtlHwnd, hwnd, rcDrag.left, rcDrag.top,
		        (rcDrag.right-rcDrag.left), (rcDrag.bottom-rcDrag.top), SWP_NOACTIVATE);
 		// adjust the drag window
 		InflateRect(&rcDrag, CHANDLESIZE, CHANDLESIZE);
 		hdwp = DeferWindowPos(hdwp, hwnd, HWND_TOP, rcDrag.left, rcDrag.top,
 		        (rcDrag.right-rcDrag.left), (rcDrag.bottom-rcDrag.top),	SWP_SHOWWINDOW);
 		EndDeferWindowPos(hdwp);
		
		cHitHandle = -1;
 
		SetCursor(LoadCursor((HINSTANCE)0,IDC_ARROW));
	
		// update GroupMoveDragRect from dragRect
		if(ClassOf(selectedCtl)!=ComboBox)
			gSetGroupMoveDragRect(objDragWindow, &dragRect, NULL);
		else {
			GetWindowRect(selectedCtlHwnd, &dragRect);
			gScreenToClientRect(TrackRect, GetParent(selectedCtlHwnd), &dragRect);
			gSetGroupMoveDragRect(objDragWindow, &dragRect, NULL);
		}
		gUpdateControlVectors(gDialog(selectedCtl), selectedCtl);
 	}
	
	// recalculate the unionized rectangle in the set
	CopyRect(&rcDrag, &cRect);  // rcDrag is used as a place hold
	gRecalculateCRect(DragWindow);         // cRect is updated
	UnionRect(&rectTmp, &rcDrag, &cRect);
	InflateRect(&rectTmp, CHANDLESIZE, CHANDLESIZE);
	InvalidateRect(GetParent(hwnd), &rectTmp, FALSE);

	if( cObjMainWindow && gIsKindOf(cObjMainWindow, Window) )
		gUpdateScrollBar(cObjMainWindow);

	return 0L;
}
 


///////////////////////////////////////////////////////////////////////////
// handleHitTest
//
// This routine takes a point from a mouse button press on a drag window
// and returns which "handle" was hit, if any.
//
// The coordinates are given in zero based coordinates of the drag
// window.
//
// Arguments:
//   HWND hwnd   - Drag window handle the x,y point is relative to.
//   int x       - Mouse X location (in the drag's client coordinates).
//   int y       - Mouse Y location (in the drag's client coordinates).
//
// Returns:
//   One of the DRAG_* constants.  If no handle was hit, the
//   return will be DRAG_CENTER.
//
///////////////////////////////////////////////////////////////////////////
static int handleHitTest(HWND hwnd,int x,int y)
{
	RECT rc;
 	int xMidStart;
 	int yMidStart;

	// Get the window rectangle and cause it to be zero-origined. 
 	GetWindowRect(hwnd, &rc);
 
	OffsetRect(&rc, -rc.left, -rc.top);
 
	// Calculate the starting points for the handles that are not on a corner. 
 	xMidStart = ((rc.right + 1) / 2) - (CHANDLESIZE / 2);
 	yMidStart = ((rc.bottom + 1) / 2) - (CHANDLESIZE / 2);
 
	if (x < CHANDLESIZE) {
 		if (y < CHANDLESIZE)
 			return DRAG_LEFTTOP;
 		else if (y > rc.bottom - CHANDLESIZE)
 			return DRAG_LEFTBOTTOM;
 		else if (y >= yMidStart && y < yMidStart + CHANDLESIZE)
 			return DRAG_LEFT;
 	}
 	else if (x > rc.right - CHANDLESIZE) {
 		if (y < CHANDLESIZE)
 			return DRAG_RIGHTTOP;
 		else if (y > rc.bottom - CHANDLESIZE)
 			return DRAG_RIGHTBOTTOM;
 		else if (y >= yMidStart && y < yMidStart + CHANDLESIZE)
 			return DRAG_RIGHT;
 	}
 	else if (x >= xMidStart && x < xMidStart + CHANDLESIZE) {
 		if (y < CHANDLESIZE)
 			return DRAG_TOP;
 		else if (y > rc.bottom - CHANDLESIZE)
 			return DRAG_BOTTOM;
	}
 
	return DRAG_CENTER;
}
 

 
///////////////////////////////////////////////////////////////////////////
// adjustRect
// 
// Adjust the RECT pRect is pointing to so that the rect height equals 
// frameWidth + n*cyChar, where cyChar is the height of the char, n an 
// integer. This adjustment works for almost every control sizing except
// for combobox case.
//
///////////////////////////////////////////////////////////////////////////
static long adjustRect(object obj, PRECT pRect)
{
	int  dy, cy, cxChar, cyChar;
 
	// Get the char width and height of the system textmetrics 
 	// and the frame width of both the top and bottom edges
 	cxCyChar(obj, &cxChar, &cyChar, &dy);
 
	// get the original height of the window
 	cy = pRect->bottom - pRect->top;
 
	// the window height after adjustment
 	cy = max(dy + (cy-dy)/cyChar*cyChar, dy+cyChar);
 
	pRect->bottom = pRect->top + cy; 
 
	return 0L;
}
 

 
static long cxCyChar(object obj, int * cxChar, int * cyChar, int * frameWidth)
{
 	HDC         hdc;
	RECT        rect1;
	HWND        hwnd=gHandle(obj);
 
	// get the frameWidth
	GetWindowRect(hwnd, &rect1);
 
	*frameWidth = rect1.bottom-rect1.top;
 
	GetClientRect(hwnd, &rect1);
 
	// get the height because of the window styles
	*frameWidth = *frameWidth - (rect1.bottom-rect1.top);
 
	*cxChar = gAveCharWidth(gGetFont(obj)); 
	*cyChar = gTextHeight(gGetFont(obj)); 
 
	return 0L; 
}
 

 
//////////////////////////////////////////////////////////////////////////////
// adjustComboRect
// 
// Adjust the RECT pRect is pointing to so that the rect height equals 
// editWidth + n*textHeight, where textHeight is the height of the text 
// in the dropdown list, n an integer. This adjustment works for combobox case.
// hwnd:    the handle to a combobox handle
//
//////////////////////////////////////////////////////////////////////////////
static long adjustComboRect(object obj, PRECT pRect, int * minHeight)
{
	int     dyEdit, textHeight, cxChar, cyChar, cy, tmp;
	RECT    rect1;
	HWND    hwnd=gHandle(obj);
 
	SendMessage(hwnd, CB_GETDROPPEDCONTROLRECT, 0, (LPARAM)((LPRECT)&rect1));
 
	textHeight = SendMessage(hwnd, CB_GETITEMHEIGHT, (WPARAM)-1, 0L);
	dyEdit = (rect1.bottom - rect1.top)%textHeight;   // the height of the frame
 
	if(textHeight <= 0) {  // use cyChar as the textHeight
 		cxCyChar(obj, &cxChar, &cyChar, &tmp);
 		textHeight = cyChar; 
	}
 
	*minHeight = dyEdit + 2*textHeight;           // the minimum height of the combobox
 
	cy = pRect->bottom - pRect->top;             // get the original height of the window
	cy = max(cy, *minHeight);
 
	// the window height after adjustment
	cy = *minHeight + (cy- (*minHeight))/textHeight*textHeight;
 
	pRect->bottom = pRect->top + cy;
 
	return 0L; 
}



/////////////////////////////////////////////////////////////////////////
// drawFrameRect
//
// This function draws a CHANDLESIZE-pixel width rectangle using the given
// raster operation.
//
// Arguments:
//   HDC   hdc   - handle to a DC of an attached dragging window
//   PRECT prc   - Rectangle to draw the frame around.
//   DWORD dwRop - RasterOp to use (PATCOPY, DSTINVERT, BLACKNESS, etc.).
//
/////////////////////////////////////////////////////////////////////////
static long drawFrameRect(HDC hdc, PRECT prc, DWORD dwRop)
{
	int      x, y;
	POINT    pt;
	HBRUSH   hBrush;
	HBITMAP  hBitmap;
	HANDLE   oldHandle; 
	COLORREF oldTextColor;
	RECT     rect;
	BYTE     bPattern[] = {0xFF, 0xFF, 0x77, 0x77, 0xFF, 0xFF, 0x77, 0x77,
							0xFF, 0xFF, 0x77, 0x77, 0xFF, 0xFF, 0x77, 0x77,
							0xFF, 0xFF, 0x77, 0x77, 0xFF, 0xFF, 0x77, 0x77,
							0xFF, 0xFF, 0x77, 0x77, 0xFF, 0xFF, 0x77, 0x77,
							0xFF, 0xFF, 0x77, 0x77, 0xFF, 0xFF, 0x77, 0x77,
							0xFF, 0xFF, 0x77, 0x77, 0xFF, 0xFF, 0x77, 0x77,
							0xFF, 0xFF, 0x77, 0x77, 0xFF, 0xFF, 0x77, 0x77,
							0xFF, 0xFF, 0x77, 0x77, 0xFF, 0xFF, 0x77, 0x77};

	if(!hdc || !prc)
		return -1L;

	CopyRect(&rect, prc);
	OffsetRect(&rect, -rect.left, -rect.top);

	hBitmap = CreateBitmap(8, 8, 1, 1, (LPVOID)bPattern);
	if(!hBitmap)
		return -1L;

	hBrush = CreatePatternBrush(hBitmap);
	if(!hBrush)	{
		DeleteObject(hBitmap);		 
		return -1L;
	}

	oldHandle = SelectObject(hdc, hBrush);
	oldTextColor = SetTextColor(hdc, RGB(0,0,128));  // Navy

	// top bar
	x = rect.right - (pt.x = rect.left);
	y = rect.bottom - (pt.y = rect.top);
	PatBlt(hdc, pt.x, pt.y, x, CHANDLESIZE, dwRop);

	// bottom bar
	pt.y = rect.bottom - CHANDLESIZE;
	PatBlt(hdc, pt.x, pt.y, x, CHANDLESIZE, dwRop);

	// left bar
	pt.y = rect.top;
	PatBlt(hdc, pt.x, pt.y, CHANDLESIZE, y, dwRop);

	// right bar
	pt.x = rect.right - CHANDLESIZE;
	PatBlt(hdc, pt.x, pt.y, CHANDLESIZE, y, dwRop);

	SetTextColor(hdc, oldTextColor);
	SelectObject(hdc, oldHandle);

	DeleteObject((HGDIOBJ) hBrush);
	DeleteObject(hBitmap);
	return 0L;
}
 


// drawOneHandle 
static void drawOneHandle(HDC hDC, int x, int y, HBRUSH hbrush) 
{ 
	RECT r; 
	r.left = x; 
	r.top = y; 
	r.right = r.left + CHANDLESIZE; 
	r.bottom = r.top + CHANDLESIZE; 
	FillRect(hDC, &r, hbrush); 
} 
 


// drawOneFrameHandle 
static void drawOneFrameHandle(HDC hDC, int x, int y, HBRUSH hBrush) 
{ 
	RECT r; 
	HBRUSH hWhiteBrush=GetStockObject(WHITE_BRUSH);
	r.left = x; 
	r.top = y; 
	r.right = r.left + CHANDLESIZE; 
	r.bottom = r.top + CHANDLESIZE;
	FillRect(hDC, &r, hWhiteBrush);  
	FrameRect(hDC, &r, hBrush); 
} 


 
//////////////////////////////////////////////////////////////////////////
// drawHandles
// 
// This routine draws the drag handles for a drag window.  The handles 
// will be solid (filled) 
// 
// Arguments: 
//   HWND    hwnd               - Drag window handle. 
//   HDC     hDC                - DC to use to draw in this window. 
//   HBITMAP hBitmap            - the bitmap handle used to fill in the handle 
//   BOOL    bFlag              - TRUE:  the handles are solid rectangles
//                                FALSE: the handles are frame rectangles
// 
////////////////////////////////////////////////////////////////////////// 
static void drawHandles(PRECT pRect, HDC hDC, BOOL bFlag) 
{ 
	RECT rc; 
	HBRUSH hBrush, hOldBrush; 
	int xMid, yMid, x2, y2; 
 
	CopyRect(&rc, pRect);
	OffsetRect(&rc, -rc.left, -rc.top); 
 
	// Precalculate some points. 
	xMid = ((rc.right + 1) / 2) - (CHANDLESIZE / 2); 
	yMid = ((rc.bottom + 1) / 2) - (CHANDLESIZE / 2); 
 
	x2 = rc.right - CHANDLESIZE; 
	y2 = rc.bottom - CHANDLESIZE; 
 
	hBrush = CreateSolidBrush(RGB(0,0,128)); 
	hOldBrush = SelectObject(hDC, hBrush); 
 
	if(bFlag) {
		drawOneHandle(hDC,0,0, hBrush);    // DRAG_LEFTTOP  
		drawOneHandle(hDC,xMid,0,hBrush);  // DRAG_TOP  
		drawOneHandle(hDC,x2,0,hBrush);    // DRAG_RIGHTTOP 
		drawOneHandle(hDC,x2,yMid,hBrush); // DRAG_RIGHT 
		drawOneHandle(hDC,x2,y2,hBrush);   // DRAG_RIGHTBOTTOM 
		drawOneHandle(hDC,xMid,y2,hBrush); // DRAG_BOTTOM 
		drawOneHandle(hDC,0,y2,hBrush);    // DRAG_LEFTBOTTOM 
		drawOneHandle(hDC,0,yMid,hBrush);  // DRAG_LEFT 
	}
	else {
		drawOneFrameHandle(hDC,0,0, hBrush);    // DRAG_LEFTTOP  
		drawOneFrameHandle(hDC,xMid,0,hBrush);  // DRAG_TOP  
		drawOneFrameHandle(hDC,x2,0,hBrush);    // DRAG_RIGHTTOP 
		drawOneFrameHandle(hDC,x2,yMid,hBrush); // DRAG_RIGHT 
		drawOneFrameHandle(hDC,x2,y2,hBrush);   // DRAG_RIGHTBOTTOM 
		drawOneFrameHandle(hDC,xMid,y2,hBrush); // DRAG_BOTTOM 
		drawOneFrameHandle(hDC,0,y2,hBrush);    // DRAG_LEFTBOTTOM 
		drawOneFrameHandle(hDC,0,yMid,hBrush);  // DRAG_LEFT 
	}

	SelectObject(hDC, hOldBrush); 
	DeleteObject(hBrush); 
} 


static void repaintCurrentDW()
{
	HWND   dragHwnd;
	object selectedCtl;

	if(!cCurrentDW)
		return;
	
	// paint the current DragWindow
	dragHwnd = gHandle(cCurrentDW);
	selectedCtl = gGetSelectedCtl(cCurrentDW);

	if(cNumOfDWs==1 && selectedCtl && ClassOf(selectedCtl)==ComboBox ) {
		RECT   rectTmp, rect;

		// erase the groupMoveDragRect of the DragWindow 
		gGetGroupMoveDragRect(cCurrentDW, &rectTmp);
		CopyRect(&rect, &rectTmp);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		InvalidateRect(GetParent(dragHwnd), &rect, FALSE);

		// redraw the dragRect of the DragWindow
		gGetDragRect(cCurrentDW, &rectTmp);
		CopyRect(&rect, &rectTmp);
		InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
		SetWindowPos(dragHwnd, HWND_TOP, rect.left, rect.top,
						rect.right-rect.left, rect.bottom-rect.top, SWP_NOMOVE);
	}

	InvalidateRect(dragHwnd, NULL, FALSE);
	UpdateWindow(dragHwnd);
	SetFocus(dragHwnd);
}


imeth object gGetSelectedCtl()
{
	return iSelectedCtl;
}


imeth HWND gGetSelectedCtlHwnd()
{
	return iSelectedCtlHwnd;
}


imeth HANDLE gHandle()
{
	return (HANDLE)iDragHwnd;
}


imeth object gGetTrackRectObj()
{
	return iTrackRectObj;
}


imeth long gSetTrackRectObj(object trackRectObj)
{
	if(iTrackRectObj==trackRectObj)
		return 0L;

	if(iTrackRectObj)
		gCancelTracking(iTrackRectObj);
		
	iTrackRectObj = trackRectObj;
	return 0L;
}



imeth int gGetDragState() 
{
	return iDragState;
}



imeth int gSetDragState(int state) 
{
	int nOldState = iDragState;
	iDragState = state;
	return nOldState;
}


imeth long gGetDragRect(PRECT pRect) 
{
	if(!pRect)
		return -1L;
	CopyRect(pRect, &iDragRect);
	return 0L;
}



imeth long gSetDragRect(PRECT pRect, PRECT pRectOld) 
{
	if(pRectOld)
		CopyRect(pRectOld, &iDragRect);

	if(!pRect)
		return -1L;
	 
	CopyRect(&iDragRect, pRect);
	return 0L;		
}


imeth long gUpdateDragRectWindow() 
{
	RECT rect;
	CopyRect(&rect, &iDragRect);
	InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);

	if(iDragHwnd)
		SetWindowPos(iDragHwnd, (HWND)0, rect.left, rect.top, 
			rect.right-rect.left, rect.bottom-rect.top, SWP_NOZORDER);
	return 0L;		
}


imeth long gGetUpdatedDragRect(PRECT pRect) 
{
	if(!iSelectedCtlHwnd || !iSelectedCtl)
		return -1L;

	// the following piece of code is needed because you may scroll or updateScrollData and
	// iDragRect may be out of date 

	if(ClassOf(iSelectedCtl)!=ComboBox) { // every type of controls except combobox
		GetWindowRect(iSelectedCtlHwnd, &iDragRect);
		gScreenToClientRect(TrackRect, GetParent(iSelectedCtlHwnd), &iDragRect);
	}
	else { // combobox only
		SendMessage(iSelectedCtlHwnd, CB_GETDROPPEDCONTROLRECT, 0, (LPARAM)((LPRECT)&iDragRect));
		gScreenToClientRect(TrackRect, GetParent(iSelectedCtlHwnd), &iDragRect);
	}

	if(!pRect)
		return -1L;

	CopyRect(pRect, &iDragRect);
	return 0L;
}



imeth long gGetGroupMoveDragRect(PRECT pRect) 
{
	if(!pRect)
		return -1L;
	CopyRect(pRect, &iGroupMoveDragRect);
	return 0L;
}


imeth long gSetGroupMoveDragRect(PRECT pRect, PRECT pRectOld) 
{
	if(pRectOld)
		CopyRect(pRectOld, &iGroupMoveDragRect);

	if(!pRect)
		return -1L;

	CopyRect(&iGroupMoveDragRect, pRect);
	return 0L;
}


imeth long gGetUpdatedGroupMoveDragRect(PRECT pRect) 
{
	if(!iSelectedCtlHwnd)
		return -1L;

	// the following piece of code is needed because you may scroll or UpdateScrollData and
	// iGroupMoveDragRect may be out of date 

	GetWindowRect(iSelectedCtlHwnd, &iGroupMoveDragRect);
	gScreenToClientRect(TrackRect, GetParent(iSelectedCtlHwnd), &iGroupMoveDragRect);

	if(!pRect)
		return -1L;

	CopyRect(pRect, &iGroupMoveDragRect);
	return 0L;
}



imeth long gMoveDragRect(int dx, int dy)
{
	iDragRect.left   += dx;
	iDragRect.right  += dx;
	iDragRect.top    += dy;
	iDragRect.bottom += dy;
	return 0L;
}



imeth long gMoveDragRectWithBoundaryCheck(int dx, int dy)
{
	int x, y;

	x = iDragRect.left;
	x += dx;
	if(x<CHANDLESIZE)
		x = CHANDLESIZE;

	iDragRect.right += (x-iDragRect.left);	
	iDragRect.left = x;

	y = iDragRect.top;
	y += dy;
	if(y<CHANDLESIZE)
		y = CHANDLESIZE;

	iDragRect.bottom += (y-iDragRect.top);
	iDragRect.top = y;

	return 0L;
}



imeth long gMoveGroupMoveDragRect(int dx, int dy)
{
	iGroupMoveDragRect.left   += dx;
	iGroupMoveDragRect.right  += dx;
	iGroupMoveDragRect.top    += dy;
	iGroupMoveDragRect.bottom += dy;
	return 0L;
}



imeth long gMoveGroupMoveDragRectWithBoundaryCheck(int dx, int dy)
{
	int x, y;

	x = iGroupMoveDragRect.left;
	x += dx;
	if(x<CHANDLESIZE)
		x = CHANDLESIZE;

	iGroupMoveDragRect.right += (x-iGroupMoveDragRect.left);	
	iGroupMoveDragRect.left = x;

	y = iGroupMoveDragRect.top;
	y += dy;
	if(y<CHANDLESIZE)
		y = CHANDLESIZE;

	iGroupMoveDragRect.bottom += (y-iGroupMoveDragRect.top);
	iGroupMoveDragRect.top = y;

	return 0L;
}



imeth long gSetWindowPositions(PRECT pRect)
{
	HDWP  hdwp;
	RECT  rect;

	if(!pRect)
		return -1L;

	CopyRect(&rect, pRect);
	gSetWindowRect(iSelectedCtl, &rect);

	// update the current DragWindow visually
	hdwp = BeginDeferWindowPos(2);
	hdwp = DeferWindowPos(hdwp, iSelectedCtlHwnd, iDragHwnd, rect.left, rect.top,
	                      0, 0, SWP_NOACTIVATE | SWP_NOSIZE);
	InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
	hdwp = DeferWindowPos(hdwp, iDragHwnd, (HWND)0, rect.left, rect.top,
		                      0, 0,	SWP_NOZORDER | SWP_NOSIZE);
	EndDeferWindowPos(hdwp);   
		
	gUpdateControlVectors(gDialog(iSelectedCtl), iSelectedCtl);
	return 0L;
}



imeth long gSetWindowSizes(PRECT pRect)
{
	HDWP  hdwp;
	RECT  rect;

	if(!pRect)
		return -1L;

	CopyRect(&rect, pRect);
	gSetWindowRect(iSelectedCtl, &rect);

	// update the DragWindow visually
	hdwp = BeginDeferWindowPos(2);
	hdwp = DeferWindowPos(hdwp, iSelectedCtlHwnd, iDragHwnd, rect.left, rect.top,
		(rect.right-rect.left), (rect.bottom-rect.top), SWP_NOACTIVATE | SWP_NOMOVE);

	InflateRect(&rect, CHANDLESIZE, CHANDLESIZE);
	hdwp = DeferWindowPos(hdwp, iDragHwnd, (HWND)0, rect.left, rect.top,
		(rect.right-rect.left), (rect.bottom-rect.top),	SWP_NOZORDER| SWP_NOMOVE);
	EndDeferWindowPos(hdwp);   

	gUpdateControlVectors(gDialog(iSelectedCtl), iSelectedCtl);
	return 0L;
}



imeth	gDispose, gDeepDispose ()
{
	object dragWindowObj;
	HWND   hwndParent;

	if(iSelectedCtl) {
		COLORREF ctlColor, winColor;
		object   dlg, ctlBackBrushObj;

		gSetDragWindow(iSelectedCtl, 0);

		// update the 3D look
		gSetModifyMode(iSelectedCtl, 0);
		ctlBackBrushObj = gGetBackBrush(iSelectedCtl);
		
		if(ctlBackBrushObj) {
			ctlColor = gColor(ctlBackBrushObj);
			dlg = gDialog(iSelectedCtl);
			if( ClassOf(iSelectedCtl)==CheckBox || ClassOf(iSelectedCtl)==RadioButton
				                                || ClassOf(iSelectedCtl)==StaticTextControl) {
				winColor = gColor( gGetBackBrush(dlg) );
			
				if (winColor != ctlColor) {
					// if the brush allocation fails, it will not change the back brush
					object objTmp;
					if (winColor == RGB(255, 255, 255)) {
						objTmp = vNew(SolidBrush, 255, 255, 255);
						if(objTmp)
							gBackBrush(iSelectedCtl, objTmp);
					}
					else {
						objTmp = vNew(SolidBrush, 192, 192, 192);
						if(objTmp)
							gBackBrush(iSelectedCtl, objTmp);
					}
				}
				ctlColor = gColor(gGetBackBrush(iSelectedCtl));
			}
			gSetHoldColor(iSelectedCtl, ctlColor);
		}

		gInvalidate3dCtl(iSelectedCtl);
		gUpdateControlVectors(dlg, iSelectedCtl); 

		if( ClassOf(iSelectedCtl)==ComboBox )
			EnableWindow(iSelectedCtlHwnd, TRUE);

 		InvalidateRect(iSelectedCtlHwnd, NULL, TRUE);
		UpdateWindow(iSelectedCtlHwnd);
	}

	if(iTrackRectObj)
		iTrackRectObj = gCancelTracking(iTrackRectObj);

	if(iDragHwnd)
		DestroyWindow(iDragHwnd);

	iDragState=DW_STATE_NORMAL;

	iSelectedCtl = 0;
	iSelectedCtlHwnd = (HWND) 0;   // this must be the last statement
	iDragHwnd = (HWND) 0;          // the handle for the dragging window

	dragWindowObj = gRemoveObj(cDWs, self);

	if(!dragWindowObj) 
		return gDispose(super);

	cNumOfDWs--;     
	
	if (cNumOfDWs<=0) {
		gDispose(cDWs);
		cCurrentDW = NULL;
		cDWs = NULL;
	}
	else if(cNumOfDWs>0 && cCurrentDW==self)
		cCurrentDW = gFirst(cDWs); 

	repaintCurrentDW();
				
	// recalculate the unionized rectangle in the set
	gRecalculateCRect(DragWindow);

	dragWindowObj = gGetToolBar(cObjMainWindow);

	if(cUpdateMenuAndToolBarFunction)
		(*cUpdateMenuAndToolBarFunction)(cObjMainWindow);

	return gDispose(super);
}


imeth gInvalidateRect(const RECT *lpRect, BOOL bErase)
{
	InvalidateRect(iDragHwnd, lpRect, bErase);
	return self;
}



 






















