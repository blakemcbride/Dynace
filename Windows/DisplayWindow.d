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




defclass  DisplayWindow : PopupWindow {
	FILE	*iFP;
	char	*iStr;
 init:	init_class;
};


private	imeth	init(FILE *fp, char *str)
{
	iFP = fp;
	iStr = str;
	return self;
}

cmeth	gNewDisplayWindow(char *name, char *file, int lines, int characters)
{
	object	wnd;
	FILE	*fp;

	fp = fopen(file, "r");
	if (!fp) {
		gMoreHandles(LowFile);
		fp = fopen(file, "r");
	}
	if (!fp)
		return NULL;
	wnd = vNew(super, name, lines, characters);
	gAutoDispose(wnd, 1);
	gSetMaxLines(wnd, 15000);
	gLoadFont(wnd, "Courier New", 10);
	return init(wnd, fp, NULL);
}

cmeth	gNewStrDisplayWindow(char *name, char *str, int lines, int characters)
{
	object	wnd;

	wnd = vNew(super, name, lines, characters);
	gAutoDispose(wnd, 1);
	gSetMaxLines(wnd, 15000);
	gLoadFont(wnd, "Courier New", 10);
	return init(wnd, NULL, str);
}

imeth	gDispose, gDeepDispose ()
{
	if (iFP)
		fclose(iFP);
	return gDispose(super);
}

imeth	int	gShow()
{
	char	buf[256], *p;
	int	i;
	long	linecount = 0L;
	
	if (iFP) {
		while (fgets(buf, sizeof buf, iFP)) {
			for (i=0, p=buf ;  p[i]  ; )
				if (p[i] == '\f') {
					p[i] = '\0';
					vPrintf(self, "%s\n", p);
					linecount++;
					p = p + i + 1;
					i = 0;
				} else
					i++;
			if (*p) {
				linecount++;
				gPuts(self, p);
			}
		}
		fclose(iFP);
		iFP = NULL;
		gScrollVert(self, (int) -(linecount * gLineHeight(gGetFont(self))));
	} else {
		for (i=0, p=iStr ;  p[i]  ; )
			if (p[i] == '\f') {
				p[i] = '\0';
				gPuts(self, p);
				gPutc(self, '\n');
				p[i] = '\f';
				linecount++;
				p = p + i + 1;
				i = 0;
			} else
				if (p[i++] == '\n')
					linecount++;
		if (*p) {
			linecount++;
			gPuts(self, p);
		}
		gScrollVert(self, (int) -(linecount * gLineHeight(gGetFont(self))));
	}
	return gShow(super);
}

static	void	init_class()
{
	gDontCollect(CLASS);
}






