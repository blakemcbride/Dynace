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


#include "windows.h"

#include "stdlib.h"

#include "jni.h"
#include "Dynace_DynaceBase.h"

void InitRefs (JNIEnv *env);

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    switch (ul_reason_for_call)
	{
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			break;
    }
    return TRUE;
}

static jmethodID midIntValue = NULL;
static jmethodID midDoubleValue = NULL;
static jmethodID midShortValue = NULL;
static jmethodID midFloatValue = NULL;

static bool bInitRefs = false;
static void *superCallAddr = NULL;

typedef void *object;
#define ClassOf(x)	(object) *((object *) (x))

static int nCnt = 0;
JNIEXPORT int JNICALL Java_Dynace_DynaceBase_getStaticCnt(JNIEnv *, jclass);
JNIEXPORT int JNICALL Java_Dynace_DynaceBase_getStaticCnt(JNIEnv *, jclass) {

	return nCnt++;
}

JNIEXPORT void JNICALL Java_Dynace_DynaceBase_setSuperCallAddr
(JNIEnv *, jclass, jint addr) {

	superCallAddr = (void *)addr;
}


int GetJavaParamData (JNIEnv *env, jshort sArgCount, int &nParmArrayCnt, short &sStrCacheArrayCnt,
					   jobject *pJArgs, char **pStrCache, int *nParms, jobjectArray vArgs) {
	jstring jszType;
	short ndx;
	for (ndx = 0; ndx < sArgCount; ndx++)
		pJArgs[ndx] = env->GetObjectArrayElement(vArgs, (jsize)ndx);

	for (ndx = sArgCount; ndx < sArgCount * 2; ndx++) {
		jszType = (jstring)env->GetObjectArrayElement(vArgs, (jsize)ndx);
		const char *pszType = env->GetStringUTFChars(jszType, NULL);
		if (!strcmp(pszType, "java.lang.String")) {
			jstring jszParm = (jstring)pJArgs[ndx-sArgCount];
			const char *pszParm = env->GetStringUTFChars(jszParm, NULL);
			char *szParmData = new char[strlen(pszParm)+1];
			strcpy(szParmData, pszParm);
			env->ReleaseStringUTFChars(jszParm, pszParm);
			nParms[nParmArrayCnt++] = (int)szParmData;
			pStrCache[sStrCacheArrayCnt++] = szParmData;
		} else if (!strcmp(pszType, "java.lang.Integer")) {
			jint jnParmVal;
			jnParmVal = env->CallIntMethod(pJArgs[ndx-sArgCount], midIntValue);
			nParms[nParmArrayCnt++] = (int)jnParmVal;
		} else if (!strcmp(pszType, "java.lang.Short")) {
			jshort jnParmVal;
			jnParmVal = env->CallShortMethod(pJArgs[ndx-sArgCount], midShortValue);
			nParms[nParmArrayCnt++] = (int)jnParmVal;
		} else if (!strcmp(pszType, "java.lang.Float")) {
			jfloat jnParmVal;
			jnParmVal = env->CallFloatMethod(pJArgs[ndx-sArgCount], midFloatValue);
			memcpy(&nParms[nParmArrayCnt++], &jnParmVal, sizeof(float));
		} else if (!strcmp(pszType, "java.lang.Double")) {
			jdouble jnParmVal;
			jnParmVal = env->CallDoubleMethod(pJArgs[ndx-sArgCount], midDoubleValue);
			memcpy(&nParms[nParmArrayCnt], &jnParmVal, sizeof(double));
			nParmArrayCnt += 2;
		} else // assume it's a java object and pass through as is
			nParms[nParmArrayCnt++] = (int)pJArgs[ndx-sArgCount];

		env->ReleaseStringUTFChars(jszType, pszType);
		env->DeleteLocalRef(jszType);
	}
	return 4 + nParmArrayCnt * 4;
}

JNIEXPORT jint JNICALL Java_Dynace_DynaceBase_callToDynace
  (JNIEnv *env, jclass cls, jint dynObj, jint dynFunc, jshort sArgCount, jobjectArray vArgs)

{
	int nRetVal;
	int nParmArrayCnt = 0;
	short sStrCacheArrayCnt = 0;
	int nParms[64];
	char *pStrCache[64];

	if (!bInitRefs)
		InitRefs(env);


	if (sArgCount > 0) {
		jobject *pJArgs = new jobject[sArgCount];
		int nStackFix = GetJavaParamData(env, sArgCount, nParmArrayCnt, sStrCacheArrayCnt, pJArgs, 
			pStrCache, nParms, vArgs);
		if (!dynObj)
			nStackFix -= 4;
		int *nLastParm = &nParms[nParmArrayCnt-1];
		_asm {
			mov		ecx, nParmArrayCnt
iter:
			mov		eax, nLastParm
			push	[eax]
			sub		nLastParm, 4
			sub		ecx, 1
			cmp		ecx, 0
			jne		iter
			cmp		dynObj, 0
			je		docall
			push	dynObj
docall:		call	dynFunc
			add		esp,nStackFix
			mov		nRetVal,eax
		}
		for (int nx = 0; nx < sStrCacheArrayCnt; nx++)
			delete pStrCache[nx];
		for (short ndx = 0; ndx < sArgCount; ndx++)
			env->DeleteLocalRef(pJArgs[ndx]);
		delete pJArgs;
	} else {
		_asm {
			push	dynObj
			call	dynFunc
			add		esp,4
			mov		nRetVal,eax
		}
	}
	
	return nRetVal;
}

JNIEXPORT jdouble JNICALL Java_Dynace_DynaceBase_callToDynaceReturnDouble
(JNIEnv *env, jclass cls, jint dynObj, jint dynFunc, jshort sArgCount, jobjectArray vArgs) 
{
	double dRet;
	int nParmArrayCnt = 0;
	short sStrCacheArrayCnt = 0;
	int nParms[64];
	char *pStrCache[64];

	if (!bInitRefs)
		InitRefs(env);
	if (sArgCount > 0) {
		jobject *pJArgs = new jobject[sArgCount];
		int nStackFix = GetJavaParamData(env, sArgCount, nParmArrayCnt, sStrCacheArrayCnt, pJArgs, 
			pStrCache, nParms, vArgs);
		if (!dynObj)
			nStackFix -= 4;
		int *nLastParm = &nParms[nParmArrayCnt-1];
		_asm {
			mov		ecx, nParmArrayCnt
iter:
			mov		eax, nLastParm
			push	[eax]
			sub		nLastParm, 4
			sub		ecx, 1
			cmp		ecx, 0
			jne		iter
			cmp		dynObj, 0
			je		docall
			push	dynObj
docall:		call	dynFunc
			fstp	dRet
			add		esp,nStackFix
		}
		for (int nx = 0; nx < sStrCacheArrayCnt; nx++)
			delete pStrCache[nx];
		for (short ndx = 0; ndx < sArgCount; ndx++)
			env->DeleteLocalRef(pJArgs[ndx]);
		delete pJArgs;
	} else {
		_asm {
			push	dynObj
			call	dynFunc
			fstp	dRet
			add		esp,4
		}
	}

	return dRet;
}

JNIEXPORT jint JNICALL Java_Dynace_DynaceBase_superCallToDynace
  (JNIEnv *env, jclass cls, jint dynObj, jint clsObj, jint dynFunc, jshort sArgCount, jobjectArray vArgs)
{
	int newFunc;
	void *findObj = (void *)clsObj;
	if (dynObj == clsObj)
		findObj = ClassOf(clsObj);

	_asm
	{
		push	2
		push	dynFunc
		push	findObj
		call	superCallAddr
		mov		newFunc, eax
		add		esp, 12
	}


	return Java_Dynace_DynaceBase_callToDynace(env, cls, dynObj, 
		newFunc, sArgCount, vArgs);
}

JNIEXPORT jdouble JNICALL Java_Dynace_DynaceBase_superCallToDynaceReturnDouble
  (JNIEnv *env, jclass cls, jint dynObj, jint clsObj, jint dynFunc, jshort sArgCount, jobjectArray vArgs)
{
	int newFunc;
	void *findObj = (void *)clsObj;
	if (dynObj == clsObj)
		findObj = ClassOf(clsObj);

	_asm
	{
		push	2
		push	dynFunc
		push	findObj
		call	superCallAddr
		mov		newFunc, eax
		add		esp, 12
	}
	return Java_Dynace_DynaceBase_callToDynaceReturnDouble(env, cls, dynObj, 
		newFunc, sArgCount, vArgs);
}

JNIEXPORT jstring JNICALL Java_Dynace_DynaceBase_getStringFromCharBuff
  (JNIEnv *env, jclass, jint pszCharBuff)
{
	char *pszBuff = (char *)pszCharBuff;
	return env->NewStringUTF(pszBuff);
}


void InitRefs (JNIEnv *env) {
	jclass cls;
	cls = env->FindClass("java/lang/Integer");
	midIntValue = env->GetMethodID(cls, "intValue", "()I");
	env->DeleteLocalRef(cls);
	cls = env->FindClass("java/lang/Double");
	midDoubleValue = env->GetMethodID(cls, "doubleValue", "()D");
	env->DeleteLocalRef(cls);
	cls = env->FindClass("java/lang/Short");
	midShortValue = env->GetMethodID(cls, "shortValue", "()S");
	env->DeleteLocalRef(cls);
	cls = env->FindClass("java/lang/Float");
	midFloatValue = env->GetMethodID(cls, "floatValue", "()F");
	env->DeleteLocalRef(cls);
	bInitRefs = true;
}

