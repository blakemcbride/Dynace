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


defclass  JavaObject : JavaClass  {
	object	iParent;
	jobject	iJavaObject;
	BOOL	iDisposeParent;
};

cmeth gNewWithJavaObj(object parent, void *jobj)
{
	JNIEnv *env;
	object	obj = gNew(super);
	accessIVsOf(obj);
	env = (JNIEnv *)gGetJavaEnv(Java);
	if (!env) {
		gDispose(obj);
		return NULL;
	}
	iJavaObject = (*env)->NewGlobalRef(env, jobj);
	iParent = parent;
	(*env)->DeleteLocalRef(env, jobj);
	return obj;
}


imeth	void	gSetDisposeParent(BOOL value)
{
	iDisposeParent = value;
}

imeth	void	*gGetJavaObject()
{
	return (void *)iJavaObject;
}

imeth	gDispose, gDeepDispose ()
{
	JNIEnv *env;
	env = (JNIEnv *)gGetJavaEnv(Java);
	if (env && iJavaObject)
		(*env)->DeleteGlobalRef(env, iJavaObject);
	if (iDisposeParent && iParent)
		gDispose(iParent);
	return gDispose(super);
}

imeth	void	*gGetJavaObj()
{
	return (void *)iJavaObject;
}

imeth	void	*gGetJavaClass()
{
	return gGetJavaObj(iParent);
}

imeth	gGetJavaClassObj()
{
	return iParent;
}

imeth	gGetJavaClassStaticMethodMap()
{
	return gGetJavaClassStaticMethodMap(iParent);
}

imeth	gGetJavaClassMethodMap()
{
	return gGetJavaClassMethodMap(iParent);
}

imeth	char	*gGetJavaClassName()
{
	return gGetJavaClassName(iParent);
}
 
