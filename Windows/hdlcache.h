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


/*  Handle types  */

#define	WINDOW_HANDLE_CACHE	0	/*  windows & controls  */
#define	DIALOG_HANDLE_CACHE	1
#define	HDC_CACHE		2
#define	CACHE_TYPES		3	/*  number of cache types  */



#define HC_NEW(typ, hwnd, obj)	gAddHandle(HandleCache, typ, (HWND) hwnd, obj)

#define HC_DELETE(typ, hwnd)	gDeleteHandle(HandleCache, typ, (HWND) hwnd)

/*
  The following code requires the save_ junk because the message loops
  which use these macros are sometimes called recursivly.  In order to
  avoid iv from changing during a recursive call it must be saved in
  a local variable.
*/

#define HC_VARS   	static	object self_save, hc_lnk = NULL; 	\
                        static	ivType *iv_save;			\
	                static	HWND	hc_hwnd;			\
			ivType	*iv=iv_save;				\
			object	self=self_save

/*  On rare occations Windows 95 passes a menu handle to a window procedure.??  */
/*  The !iv check below is designed to handle that condition.  */
/*  The topmost comment is not true.  95 draws different handle IDs (i.e. window vs. menu) from
    different handle pools so it is possible for a menu and window handle to have the same value.  */

#define HC_UPDATE(typ, h)  if (!hc_lnk  ||  hc_hwnd != (HWND) h)					\
				if (self=self_save=gAddCache(HandleCache, typ, (HWND) h, &hc_lnk)) {	\
					iv_save = iv = ivPtr(self);				\
					if (!iv) {						\
						self_save = self = hc_lnk = NULL;		\
						hc_hwnd = 0;					\
					} else							\
						hc_hwnd = (HWND) h;				\
				}  else  {							\
					iv_save = iv = NULL;					\
					hc_hwnd = 0;						\
				}





