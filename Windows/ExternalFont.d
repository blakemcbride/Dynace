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


defclass  ExternalFont : Font  {
	int	iCopies;	/*  number of copies of the font  */
};

#include <string.h>
#include <math.h>


static	void	removeLast(char *v);
static	char	*getLast(char *v);
static	HGDIOBJ	getLogfont(char *name, int ptSize, HDC hdc);


cmeth	gNewExtFontWithHDC(char *name, int ptSize, HDC hdc)
{
	HGDIOBJ		hFont;
	object		obj;

	hFont = getLogfont(name, ptSize, hdc);
	if (!hFont)
		return NULL;
	obj = vNew(super, hFont);
	ivPtr(obj)->iCopies = 1;
	return obj;
}

#if 0
//cmeth	gNewExtFontWithHDC(char *name, int ptSize, HDC hdc)
{
	HGDIOBJ		hFont;
	LOGFONT		lf;
	object		obj;
	POINT		p;

	memset(&lf, 0, sizeof lf);

	SaveDC(hdc);
	SetGraphicsMode(hdc, GM_ADVANCED);
	ModifyWorldTransform(hdc, NULL, MWT_IDENTITY);
	SetViewportOrgEx(hdc, 0, 0, NULL);
	SetWindowOrgEx(hdc, 0, 0, NULL);
	
	if (IsObj((object)name))
		name = gStringValue((object)name);

	strcpy(lf.lfFaceName, name);

	p.y = 10 * ptSize * (float) GetDeviceCaps(hdc, LOGPIXELSY) / 72;
	p.x = 0;
	DPtoLP(hdc, &p, 1);


	lf.lfHeight = -(int)(fabs(p.y) / 10.0 + .5);

	lf.lfWeight = 700;

	hFont = CreateFontIndirect(&lf);
	RestoreDC(hdc, -1);
	if (!hFont)
		return NULL;
	obj = vNew(super, hFont);
	ivPtr(obj)->iCopies = 1;
	return obj;
}
#endif

cmeth	gNewExternalFont(char *name, int ptSize)
{
	return vNew(self, name, ptSize);
}

cvmeth	vNew(char *name,		/*  or may be object  */
	     int ptSize)
{
	HGDIOBJ		hFont;
	object		obj;

	hFont = getLogfont(name, ptSize, (HDC) 0);
	if (!hFont)
		return NULL;
	obj = vNew(super, hFont);
	ivPtr(obj)->iCopies = 1;
	return obj;
}

cmeth	gIndirect(LOGFONT *lf)
{
	HGDIOBJ		hFont;
	object		obj;

	hFont = CreateFontIndirect(lf);
	if (!hFont)
		return NULL;
	obj = vNew(super, hFont);
	ivPtr(obj)->iCopies = 1;
	return obj;
}

imeth	object	gDispose, gDeepDispose ()
{
	if (--iCopies)
		return NULL;
	DeleteObject(gHandle(self));
	return gDispose(super);
}

imeth	object	gGCDispose()
{
	DeleteObject(gHandle(self));
	return gDispose(super);
}

imeth	gCopy, gDeepCopy ()
{
	iCopies++;
	return self;
#if 0
	LOGFONT		lf;

	GetObject(gHandle(self), sizeof(lf), (LPSTR) &lf);
	return gIndirect(ClassOf(self), &lf);
#endif
}

static	char	*getLast(char *v)
{
	int	i = strlen(v) - 1;
	for ( ; i > 0  &&  v[i] != ' ' ; i--);
	return i ? v + i + 1 : v;
}

static	void	removeLast(char *v)
{
	int	i = strlen(v) - 1;
	for ( ; i > 0  &&  v[i] != ' ' ; i--);
	if (i >= 0)
		v[i] = '\0';
}

static	HGDIOBJ	getLogfont(char *name, int ptSize, HDC hdc)
{
	LOGFONT		lf;
	object		obj;
	char		buf[80];

	memset(&lf, 0, sizeof(LOGFONT));

	if (IsObj((object)name))
		name = gStringValue((object)name);

//  The following decoding of the font attributes isn't needed under NT but is under 95. ??

	strcpy(buf, name);
	while (buf[0]) {
		char	*p;
		
		p = getLast(buf);
		if (!stricmp(p, "bold"))
			lf.lfWeight = 700;
		else if (!stricmp(p, "italic"))
			lf.lfItalic = 1;
		else if (!stricmp(p, "underline"))
			lf.lfUnderline = 1;
		else if (!stricmp(p, "strikeout"))
			lf.lfStrikeOut = 1;
		else
			break;
		removeLast(buf);
	}

	strncpy(lf.lfFaceName, buf, LF_FACESIZE);
	lf.lfFaceName[LF_FACESIZE-1] = '\0';

	if (hdc)
		lf.lfHeight = -MulDiv(ptSize, GetDeviceCaps(hdc, LOGPIXELSY), 72);
	else
		lf.lfHeight = -MulDiv(ptSize, gPixelHeight(CLASS), 72);

	return CreateFontIndirect(&lf);
}






