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


#ifndef	_WIN32
void	Sleep(DWORD millisecs);
#endif


/* Standard mask formatting functions in the text control. */

#define MASK_FORMAT_NONE	0
#define MASK_FORMAT_PHONE	1
#define	MASK_FORMAT_ZIPCODE	2
#define	MASK_FORMAT_SSNUMBER	3


/*  Scaling modes   */

#define SM_10_PER_SYSCHAR	0	/*  10 points per system character  */
#define SM_PIXELS		1	/*  streight pixels                 */
#define SM_1_PER_CHAR		2	/*  1 per a particlar font          */


/*  Print Dialog modes  */
#define PM_LETTER	0x0000L
#define PM_LEGAL	0x0001L
#define PM_PORTRAIT	0x0000L
#define PM_LANDSCAPE	0x0002L


/*  Bitmap positioning options  */
#define	BMP_NONE	0
#define	BMP_POSITION	1
#define	BMP_CENTER	2
#define	BMP_TILED	3
#define	BMP_FILLWIND	4


/*  ODBC Database types  */
#define	DBMS_WATCOM	1	/*  Sybase SQL Anywhere   */
#define	DBMS_ACCESS	2	/*  MS Access Database	  */
#define	DBMS_SYBASE	3	/*  Sybase SQL Server     */
#define	DBMS_MSSQL	4	/*  Microsoft SQL Server  */
#define	DBMS_MYSQL	5	/*  MySQL Database	  */
#define	DBMS_EXCEL	6	/*  Excel Spreadsheet	  */
#define	DBMS_TEXT	7	/*  Text Files		  */
#define	DBMS_POSTGRES	8	/*  PostgreSQL		  */
#define DBMS_ORACLE	9	/* Oracle */


#define mLoad(c, i)		gLoad(c, (unsigned) (i))
#define mLoadMenu(w, i)		gLoadMenu(w, (unsigned) (i))
#define mAssociate(w, i, f)	gAssociate(w, (UINT) (i), (void (*)()) f)
#define mMenuFunction(w, i)	gMenuFunction(w, (UINT) (i))
#define mSetMode(h, i, m)	gSetMode(h, (UINT) (i), m)
#define mMenuItemMode(w, i, m)	gMenuItemMode(w, (unsigned) (i), m)
#define mLoadCursor(w, i)	gLoadCursor(w, (unsigned) (i));
#define mLoadIcon(w, i)		gLoadIcon(w, (unsigned) (i));

#define mNewDialog(c, i, w)	gNewDialog(c, (WORD) (i), w)
#define mAddControl(s,c,i)	gAddControl(s, c, (UINT) (i))
#define mAddControlStr(s,c,i,n)	gAddControlStr(s, c, (UINT) (i), n)
#define mGetControl(s,i)	gGetControl(s, (UINT) (i))
#define mCtlValue(s,i)		gCtlValue(s, (UINT) (i))
#define mIndexValue(s,i)	gIndexValue(s, (UINT) (i))
#define mCtlStringValue(s,i)	gCtlStringValue(s, (UINT) (i))
#define mCtlShortValue(s,i)	gCtlShortValue(s, (UINT) (i))
#define mCtlUnsignedShortValue(s,i)	gCtlUnsignedShortValue(s, (UINT) (i))
#define mCtlLongValue(s,i)	gCtlLongValue(s, (UINT) i)
#define mCtlDoubleValue(s,i)	gCtlDoubleValue(s, (UINT) (i))
#define mChangeMenuText(s,i,t)	gChangeMenuText(s, (UINT) (i), t)
#define	mAddToolBitmap(s,id1,id2,space,ifun,tip)	gAddToolBitmap(s, (UINT) (id1), (UINT) (id2), space, ifun, tip)

#define mNewRadioButton(c, d, i, n)		gNewRadioButton(c, d, (UINT) (i), (UINT) (n))
#define mNewRadioButtonStr(c, d, i, n, s)	gNewRadioButtonStr(c, d, (UINT) (i), (UINT) (n), s)

#define mNewCtl(s, i)		vNew(s, (UINT) (i))
#define mGroup(s, i, j)		gGroup(s, (unsigned) (i), (unsigned) (j))

/*  Resource reading code  */

HWND  ResourceDialogParam(char * ResFileName, LPCSTR lpName, HWND   parent, DLGPROC pDialogProc, LPARAM  param);
HMENU ResourceLoadMenu(char *ResFileName, char *id);
HICON ResourceLoadIcon(char *ResFileName, char *id);
HCURSOR ResourceLoadCursor(char *ResFileName, char *id);
char *ResourceLoadBitmap(char *ResFileName, char *id);






