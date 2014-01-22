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


defclass DatePicker {
	object	iDateCtl;
	object	iButtonCtl;
	object	iWind;
	long	iResultDate;
	long	iCurrentYearMonth;
	long	iCurrentDay;
};

#include "color.h"

#define Msg(s) gMessage(Application, s)

private	imeth	object	pLoadWindow(object self);
private	imeth	object	pUpdateWindow(object self);

static	long	process_wm_char(object	wind, 
				HWND	hwnd, 
				UINT	mMsg, 
				WPARAM	wParam, 
				LPARAM	lParam)
{
	if ((int) wParam == 27) {
		gEndPerform(wind, 0);
		return 0L;
	}
	return gCallDefaultProc(wind, mMsg, wParam, lParam);
}

private	imeth	pInitInstance(object self, object dctl, object bctl)
{
	iDateCtl = dctl;
	iButtonCtl = bctl;
//	if (dctl)
//		gSetMouseFunction(dctl, (MK_SHIFT | MK_RBUTTON), selectDateFromDateCtl);
//	if (bctl)
//		gSetFunction(bctl, selectDateFromButton);
	return self;
}

cmeth	gNewDatePicker(object dctl, object bctl)
{
	return pInitInstance(gNew(super), dctl, bctl);
}

private	imeth	int	pCloseDatePicker(object self)
{
	object	parent = gGetParent(iWind);

	if (parent)
		gEnable(parent);
	gEndPerform(iWind, !!iResultDate);
	
	return 1;
}

static	int	closeDatePicker(object wind)
{
	return pCloseDatePicker(gPropertyGet(wind, "DatePicker"));
}

private	imeth	int	pSelectDay(object self, int day)
{
	if (day) {
		iResultDate = ((iCurrentYearMonth / 100L) * 100L) + (long) day;
		iCurrentDay = day;
	
		gEndPerform(iWind, 1);
	}

	return 0;
}

static	int	selectDay(object ctl, object wind)
{
	return pSelectDay(gPropertyGet(wind, "DatePicker"), atoi(gStringValue(ctl)));
}

static	object	addRectControlWithName(object wind, int y, int x, int h, int w, char *name, char *text,
			       int tcolor, int bcolor, int fcolor, char *fnt, int sz)
{
	object	ctl = gAddRectControl(wind, y, x, h, w, name, text);

	gAllowEdge(ctl, 1);
	gSetFrameColor(ctl, (char) fcolor);
	gSetFont(ctl, vNew(ExternalFont, fnt, sz));
	gSetFill(ctl, (char) 1);
	gSetPattern(ctl, 1, 1, (char) bcolor, (char) bcolor);
	gSetFrameThickness(ctl, (char) 1);
	gSetTextColor(ctl, (char) tcolor);
	gSetFrameStyle(ctl, (char) FRAME_Full);
	return ctl;
}

static	object	addRectControl(object wind, int y, int x, int h, int w, char *text,
			       int tcolor, int bcolor, int fcolor, char *fnt, int sz)
{
	char	name[256];

	sprintf(name, "CTL-%s", text);

	return addRectControlWithName(wind, y, x, h, w, name, text, tcolor, bcolor, fcolor, fnt, sz);
}

static	object	addDateRectControl(object wind, int y, int x, char *text, int tcolor, int bcolor, int is_3d)
{
	object	rval = addRectControl(wind, y, x, 20, 25, text, tcolor, bcolor, CLR_White, "Ariel", 8);
	
	gDraw3D(rval, is_3d);
	
	return rval;
}	

private	imeth	int	pMoveYear(object self, object wind, int val)
{
	object	dtobj = gNewWithLong(Date, iCurrentYearMonth);

	gAddYears(dtobj, val);
	iCurrentYearMonth = gLongValue(dtobj);
	gDispose(dtobj);

	pUpdateWindow(self);

	return 0;
}

static	int	prevYear(object ctl, object wind)
{
	return pMoveYear(gPropertyGet(wind, "DatePicker"), wind, -1);
}

static	int	nextYear(object ctl, object wind)
{
	return pMoveYear(gPropertyGet(wind, "DatePicker"), wind, 1);
}

private	imeth	int	pSelectMonth(object self, int month)
{
	long	year = (int) (iCurrentYearMonth / 10000L);

	iCurrentYearMonth = (year * 10000L) + ((long) month * 100L) + 1L;

	pUpdateWindow(self);

	return 0;
}

static	int	selectMonth(object ctl, object wind)
{
	return pSelectMonth(gPropertyGet(wind, "DatePicker"), gShortValue(gPropertyGet(ctl, "Month")));
}

private	imeth	int	pGoToToday(object self)
{
	object	dtobj = gToday(Date);
	long	today = gLongValue(dtobj);
	

	iCurrentYearMonth = ((today / 100L) * 100L) + 1;
	iCurrentDay = (today - iCurrentYearMonth) + 1;

	gDispose(dtobj);
	pUpdateWindow(self);

	return 0;
}

static	int	goToToday(object ctl, object wind)
{
	return pGoToToday(gPropertyGet(wind, "DatePicker"));
}

private	imeth	object	pLoadWindow(object self)
{
	long	dt = iCurrentYearMonth;
	int	year = (int) (dt / 10000L);
	int	month = (int) ((dt % 10000L) / 100L);
	object	ctl;
	char	buf[10];
	int	i, j, clr, x, y, val;
	char	*mths[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
	char	*days[7] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
	object	dtobj = gToday(Date);
	long	today = gLongValue(dtobj);
	int	tyear = (int) (today / 10000L);
	int	tmonth = (int) ((today % 10000L) / 100L);
	int	tday = (int) (today % 100L);
	int	dow;
	int	day = 0;
	int	done;
	int	lday;
	int	dct = 0;

	gDispose(dtobj);
	
	gEraseAll(iWind);

	ctl = addRectControl(iWind, 5, 5, 20, 35, "Today", CLR_Black, CLR_White, CLR_Orange, "Ariel Bold", 7);
	gSetFunction(ctl, goToToday);
	
	x = 45;
	ctl = addRectControl(iWind, 5, x, 20, 25, "<", CLR_Orange, CLR_White, CLR_Orange, "Ariel Bold", 10);
	gSetFunction(ctl, prevYear);

	x += 25;
	sprintf(buf, "%d", year);
	ctl = addRectControlWithName(iWind, 5, x, 20, 50, "CTL-Year", buf,
				     CLR_Black, CLR_White, CLR_Orange, "Ariel Bold", 10);

	x+= 50;
	ctl = addRectControl(iWind, 5, x, 20, 25, ">", CLR_Orange, CLR_White, CLR_Orange, "Ariel Bold", 10);
	gSetFunction(ctl, nextYear);

	for (i = 0; i < 12 ; i++) {
		val = 29;
		if (i == month - 1)
			clr = CLR_Orange;
		else
			clr = CLR_Black;
		if (i < 6) {
			y = 30;
			x = 5 + (i * val);
		} else {
			y = 45;
			x = 5 + ((i - 6) * val);
		}

		ctl = addRectControl(iWind, y, x, 15, val, mths[i], clr, CLR_White, CLR_White, "Ariel", 7);
		gPropertyPut(ctl, "Month", 1, gNewWithInt(ShortInteger, i + 1));
		gSetFunction(ctl, selectMonth);
	}

	for (i = 0; i < 7 ; i++) {
		val = 25;
		x = 5 + (i * val);

		ctl = addRectControl(iWind, 65, x, 15, val, days[i], CLR_White, CLR_Black, CLR_White, "Ariel", 7);
	}

	done = 0;
	dtobj = gNewWithLong(Date, dt);
	dow = (int) (gJulian(dtobj) % 7L);
	gAddMonths(dtobj, 1);
	gAddDays(dtobj, -1);
	lday = (int) (gLongValue(dtobj) % 100L);
	if (iCurrentDay > lday)
		iCurrentDay = lday;
	gDispose(dtobj);
	y = 80;
	for (j = 0; j < 6; j++) {
		if (j)
			y += 20;
		for (i = 0; i < 7; i++) {
			val = 25;
			x = 5 + (i * val);

			if (!done && !day && dow == i)
				day = 1;
			else if (day >= lday) {
				done = 1;
				day = 0;
			} else if (day)
				day++;
			if (year == tyear && month == tmonth && day == tday)
				clr = CLR_White;
			else if (day && (!i || i == 6))
				clr = CLR_Gray;
			else
				clr = CLR_Silver;
			if (day)
				sprintf(buf, "%d", day);
			else
				*buf = '\0';
			ctl = addDateRectControl(iWind, y, x, buf, CLR_Black, clr, day == iCurrentDay);
			sprintf(buf, "D%d", dct++);
			gSetName(ctl, buf);
			gSetFunction(ctl, selectDay);
		}
	}
	
	gPerform(iWind);
	
	return self;
}

private	imeth	object	pUpdateWindow(object self)
{
	long	dt = iCurrentYearMonth;
	int	year = (int) (dt / 10000L);
	int	month = (int) ((dt % 10000L) / 100L);
	object	ctl;
	char	buf[100];
	int	i, j, clr;
	char	*mths[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
	char	*days[7] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
	object	dtobj = gToday(Date);
	long	today = gLongValue(dtobj);
	int	tyear = (int) (today / 10000L);
	int	tmonth = (int) ((today % 10000L) / 100L);
	int	tday = (int) (today % 100L);
	int	dow;
	int	day = 0;
	int	done;
	int	lday;
	int	dct = 0;

	gDispose(dtobj);
	
	sprintf(buf, "%d", year);
	ctl = gGetControlStr(iWind, "CTL-Year");
	gSetStringValue(ctl, buf);

	for (i = 0; i < 12 ; i++) {
		if (i == month - 1)
			clr = CLR_Orange;
		else
			clr = CLR_Black;

		sprintf(buf, "CTL-%s", mths[i]);
		ctl = gGetControlStr(iWind, buf);
		gSetTextColor(ctl, (char) clr);
	}

	done = 0;
	dtobj = gNewWithLong(Date, dt);
	dow = (int) (gJulian(dtobj) % 7L);
	gAddMonths(dtobj, 1);
	gAddDays(dtobj, -1);
	lday = (int) (gLongValue(dtobj) % 100L);
	if (iCurrentDay > lday)
		iCurrentDay = lday;
	gDispose(dtobj);
	for (j = 0; j < 6; j++) {
		for (i = 0; i < 7; i++) {
			if (!done && !day && dow == i)
				day = 1;
			else if (day >= lday) {
				done = 1;
				day = 0;
			} else if (day)
				day++;
			if (year == tyear && month == tmonth && day == tday)
				clr = CLR_White;
			else if (day && (!i || i == 6))
				clr = CLR_Gray;
			else
				clr = CLR_Silver;
			sprintf(buf, "D%d", dct++);
			ctl = gGetControlStr(iWind, buf);
			if (day)
				sprintf(buf, "%d", day);
			else
				*buf = '\0';
			gDraw3D(ctl, iCurrentDay == day);
			gSetStringValue(ctl, buf);
			gSetPattern(ctl, 1, 1, (char) clr, (char) clr);
		}
	}
	gRedrawWindow(iWind);
	
	return self;
}

imeth	long	gGetDate()
{
	int	x, y;
	int	sm = gSetScalingMode(Application, SM_PIXELS);
	object	pwind = NULL;
	long	rval = 0L;
	object	ctl;
	
	iCurrentYearMonth = 0L;
	iResultDate = 0L;
	if (iDateCtl) {
		iCurrentYearMonth = gLongValue(iDateCtl);
		pwind = gDialog(iDateCtl);
	} else if (iButtonCtl)
		pwind = gDialog(iButtonCtl);
	if (!iCurrentYearMonth) {
		object	dtobj = gToday(Date);
		
		iCurrentYearMonth = gLongValue(dtobj);
		gDispose(dtobj);
	}
	iCurrentDay = iCurrentYearMonth;
	iCurrentYearMonth = ((iCurrentYearMonth / 100L) * 100L) + 1;
	iCurrentDay = (iCurrentDay - iCurrentYearMonth) + 1;

	iWind = vNew(PopupWindow, "Date Picker", 235, 190);
	gPropertyPut(iWind, "DatePicker", 0, self);
	gSetStyle(iWind, (WS_VISIBLE | WS_BORDER | WS_POPUPWINDOW | WS_DLGFRAME)
		  & ~WS_SYSMENU);
//		  & ~WS_MINIMIZEBOX & ~WS_MAXIMIZEBOX);
	gAddHandlerAfter(iWind, (unsigned) WM_CHAR, process_wm_char);
	gDefaultProcessingMode(iWind, (unsigned) WM_CHAR, 0);  /*  no auto default processing  */
	
// 	if (iDateCtl) {
// 		gGetPosition(iDateCtl, &y, &x);
// 		gSetPosition(iWind, y, x);
// 	} else if (iButtonCtl) {
// 		gGetPosition(iButtonCtl, &y, &x);
// 		gSetPosition(iWind, y, x);
// 	} else
 		gCenter(iWind);

	gCompletionFunction(iWind, closeDatePicker);

	if (pwind) {
		gSetParent(iWind, pwind);
		gDisable(pwind);
	}

	pLoadWindow(self);
	
	gPerformModal(iWind);

	gDispose(iWind);
	rval = iResultDate;

	return rval;
}

cmeth	long	gPickDate(object dctl, object bctl)
{
	object	obj = pInitInstance(gNew(super), dctl, bctl);
	long	rval = gGetDate(obj);

	gDispose(obj);

	return rval;
}
	



