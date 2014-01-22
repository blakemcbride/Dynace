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

#include <stdlib.h>

#include "oleserv2.h"

defclass Server {

	char	iString[30];
	short	iShort;
	
init:	class_init;
};


extern	object	mainWind;

cmeth	gNewOleInstance(DISPPARAMS *params, VARIANT *res)
{
	object	obj = gNew(super);
//	accessIVsOf(obj);
	return obj;
}

private	imeth	setValues(DISPPARAMS *params, VARIANT *res)
{
	//  get arguments passed and use the values
	strcpy(iString, GetString(1));
	iShort = GetShort(2);

	//  return a single value of any type
	SetShort(0);
	
	return self;
}

private	imeth	getString(DISPPARAMS *params, VARIANT *res)
{
	//  return a single value of any type
	SetString(iString);
	
	return self;
}


static	void	class_init()
{
	gAddMethod(OLEDispatch, CLASS, "gSetValues", setValues);
	gAddMethod(OLEDispatch, CLASS, "gGetString", getString);
}



