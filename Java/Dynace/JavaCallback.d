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
#include "jni.h"


defclass  JavaCallback {
	object	callbackObj;
	object	methodName;
	object	methodSig;
};

private imeth pInitInstance(object self, jobject jobj, char *clsName, 
				char *methName, char *methSig);


private imeth pInitInstance(object self, jobject jobj, char *clsName, 
				char *methName, char *methSig)
{
	callbackObj = gAttachJavaObject(JavaClass, clsName, jobj);
	methodName = gNewWithStr(String, methName);
	methodSig = gNewWithStr(String, methSig);
	return self;
}

cmeth	gNewJavaCallback(void *jobj, char *clsName, char *methName, char *methSig) 
{
	return pInitInstance(gNew(super), (jobject)jobj, clsName, methName, methSig);
}

cmeth	gLoadJavaClass(char *className)
{
	if (!className)
		return NULL;
	if (*className != '!')
		return NULL;
	return gNewWithStr(JavaClass, (className + 1));
}

imeth	gDispose, gDeepDispose ()
{
	if (methodName)
		gDispose(methodName);
	if (methodSig)
		gDispose(methodSig);
	if (callbackObj)
		gDispose(callbackObj);
	return gDispose(super);
}

cmeth	char	*gGetLastJavaError()
{
	return gGetLastJavaError(Java);
}

imeth	int	gPerformJavaMenuCallback(object wnd, int rescID)
{
	BOOL bErr;
	return vCallJavaMethod(callbackObj,
			       gStringValue(methodName),
			       gStringValue(methodSig), &bErr,
			       wnd, rescID);
}

imeth	int	gPerformJavaObjObjCallback(object obj1, object obj2)
{
	BOOL bErr;
	return vCallJavaMethod(callbackObj,
			       gStringValue(methodName),
			       gStringValue(methodSig), &bErr,
			       obj1, obj2);
}

imeth	int	gPerformJavaObjCallback(object obj1)
{
	BOOL bErr;
	return vCallJavaMethod(callbackObj,
			       gStringValue(methodName),
			       gStringValue(methodSig), &bErr,
			       obj1);
}

imeth	gPerformJavaCheckValueCallback(object obj1, object obj2)
{
	BOOL bErr;
	return (object)vCallJavaMethod(callbackObj,
			       gStringValue(methodName),
			       gStringValue(methodSig), &bErr,
			       obj1, obj2);
}
