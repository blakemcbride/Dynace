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

#include <stdlib.h>
#include "jni.h"
#include "package.h"

defclass  JavaClass  {
	jclass		iClass;
	object		iMethodIDMap; 		/* key format is sig@name */
	object		iStaticMethodIDMap; 	/* key format is sig@name */
	object		iJavaClassName;

class:
	cJavaClassMap;
	cTwinClassMap;
	cJavaTwinGenericMap;
 init:	class_init;
};

#define	JAVAERRORLOG	"JavaErrorLog.txt"

#define	MAXARGS		32

private imeth	jmethodID	gFindJavaMethod(object self, char *methodName, 
								char *methodsig, BOOL isStatic);
private imeth	int	pCallMethod(object self, JNIEnv *env, void *fp, jmethodID mid, void *javaobj, 
					jvalue *nParms, int nParmCnt, int *excpFlag);
private imeth	double	pCallDoubleMethod(object self, JNIEnv *env, void *fp, jmethodID mid, void *javaobj, 
					jvalue *nParms, int nParmCnt, int *excpFlag);

private	imeth	int	pSetupJNIMethodCall(char *methodname, char *methodsig, jmethodID *mid, BOOL *isStatic,
					    int *lastparm, jvalue *nParms,
					    int *nParmIntCnt, object *dynDisposeParms,
					    int *dynDisposeParmCnt, jstring *jsParm, int *stringParmCnt);
private	imeth	void	SetErrorMsg(char *methodname, char *methodsig, BOOL isStatic, char *message, int excpFlag);

private	imeth	void	pGetJNIFunction(char *methodsig, BOOL isStatic, void **vpFunc,
				       BOOL *isStringMethod, BOOL *isObjectMethod, BOOL *isDynObjMethod);

static	char	*FixClassName(char *clsname, char *buf);
private	imeth	setupInstanceVars(jclass jcls, char *clsname, int DelLocalRef);

private	imeth	int	isTwinClass(char *clsname);

static	object	JavaTwinClass_im_gDisposeWithCls(object cls, object self);

static byte gDispose_asmblock[] = {
	0x8B, 0xCC,			    //mov     ecx, esp
	0x83, 0xC1, 0x04,		    //add     ecx, 4
	0xFF, 0x31,			    //push    dword ptr [ecx]
	0x68, 0xFF, 0xFF, 0xFF, 0xFF,       //push    cls
	0xB8, 0xFF, 0xFF, 0xFF, 0xFF,	    //mov     eax, JavaTwinClass_im_gDisposeWithCls
	0xFF, 0xD0, 			    //call    eax
	0x83, 0xC4, 0x08,	            //add     esp,8
	0xC3				    //ret
};
	
static byte asmblock[] = {
	0x5A,				    //pop     edx
	0x89, 0x15, 0xFF, 0xFF, 0xFF, 0xFF, //mov     dword ptr [pretaddrsave], edx
	0x89, 0x25, 0xFF, 0xFF, 0xFF, 0xFF, //mov     dword ptr [pstacksave], esp
	0x59,				    //pop     ecx
	0x8B, 0xC1,                	    //mov     eax,ecx
	0x8B, 0x08,		            //mov     ecx,dword ptr [eax]
	0x51,		                    //push    ecx
	0x50,				    //push    eax
	0xFF, 0x15, 0xFF, 0xFF, 0xFF, 0xFF, //call    dword ptr [_GetIVptr]
	0x83, 0xC4, 0x08,	            //add     esp,8
	0x68, 0xFF, 0xFF, 0xFF, 0xFF, 	    //push    _pbErr
	0x68, 0xFF, 0xFF, 0xFF, 0xFF,       //push    _methsig
	0x68, 0xFF, 0xFF, 0xFF, 0xFF,       //push    _methname
	0xFF, 0x30,			    //push    dword ptr [eax]
	0xFF, 0x15, 0xFF, 0xFF, 0xFF, 0xFF, //call    dword ptr [_vCallJavaMethod]
	0x8B, 0x25, 0xFF, 0xFF, 0xFF, 0xFF, //mov     esp,dword ptr [_pstacksave]
	0xFF, 0x35, 0xFF, 0xFF, 0xFF, 0xFF, //push    dword ptr [_pretaddrsave]
	0xC3				    //ret
						};

static	char	*FixClassName(char *clsname, char *buf)
{
	char	*p = clsname, *t;

	for (t=buf ; *p ; p++)
		*t++ = *p == '.' ? '/' : *p;
	*t = '\0';
	return buf;
}

static	char	*ExtractClassName(char *clsname, char *buf)
{
	char	*p, *t;

	for (p=clsname ; *p  &&  *p != ')' ; ++p);
	if (*p) {
		if (*++p == 'L')
			++p;
	} else
		p = clsname;
	for (t=buf ; *p ; p++)
		if (*p != ';')
			*t++ = *p == '.' ? '/' : *p;
	*t = '\0';
	return buf;
}

#define	CheckStatic(staticfunc, instancefunc) \
                if (isStatic) \
                        *vpFunc = (*env)->staticfunc; \
                else \
                        *vpFunc = (*env)->instancefunc

private	imeth	void	pGetJNIFunction(char *methodsig, BOOL isStatic, void **vpFunc,
				       BOOL *isStringMethod, BOOL *isObjectMethod, BOOL *isDynObjMethod)
{
	char	*returnTypePos = strrchr(methodsig, ')');
	JNIEnv	*env = (JNIEnv *)gGetJavaEnv(Java);
	if (!returnTypePos)
		return;
	returnTypePos++;
	if (!strcmp(returnTypePos, "I")) {
		CheckStatic(CallStaticIntMethodA, CallIntMethodA);
	} else if (!strcmp(returnTypePos, "V")) {
		CheckStatic(CallStaticVoidMethodA, CallVoidMethodA);
	} else if (!strcmp(returnTypePos, "D")) {
		CheckStatic(CallStaticDoubleMethodA, CallDoubleMethodA);
	} else if (!strcmp(returnTypePos, "F")) {
		CheckStatic(CallStaticFloatMethodA, CallFloatMethodA);
	} else if (!strcmp(returnTypePos, "S")) {
		CheckStatic(CallStaticShortMethodA, CallShortMethodA);
	} else if (!strcmp(returnTypePos, "C")) {
		CheckStatic(CallStaticCharMethodA, CallCharMethodA);
	} else {
		CheckStatic(CallStaticObjectMethodA, CallObjectMethodA);
		if (!strcmp(returnTypePos, "Ljava/lang/String;"))
			*isStringMethod = 1;
		else {
			int	isDynObjRet = 0;
			char	className[256];
			/*  extract class name from return type signature */
			strcpy (className, returnTypePos + 1);
			className[strlen(className) - 1] = '\0';
			if (!strcmp(className, gGetObjectPackageClassName(Java)))
				isDynObjRet = 1;
			else
				isDynObjRet = isTwinClass(self, className);
			
			if (isDynObjRet)
				*isDynObjMethod = 1;
			else
				*isObjectMethod = 1;
		}
	}
}

private	imeth	void	SetErrorMsg(char *methodname, char *methodsig, BOOL isStatic, char *message, int excpFlag)
{
	char *staticgflag = isStatic ? " Static " : " ";
	object	msg = vBuild(String, "Error Calling", staticgflag, "Java Method. message=",
			     message, ". class=",
			     gGetJavaClassName(self), ". method=", methodname,
			     ". signature=", methodsig , END);
	gSetLastJavaError(Java, msg);
}

#define	SetupJNIMethodCallLocals() \
        object 		dynDisposeParms[MAXARGS]; \
	int		stringParmCnt = 0, dynDisposeParmCnt = 0;  \
	jvalue		nParms[MAXARGS];  \
	int		nParmIntCnt = 0;   \
	jstring 	jsParm[MAXARGS]; \
	BOOL 		isStatic = FALSE;  \
        void		*vpFunc = NULL; \
	JNIEnv		*env = (JNIEnv *)gGetJavaEnv(Java);   \
        jmethodID	mid; \
        int		returnval = -1; \
        int		isStringMethod = 0; \
        int		isObjectMethod = 0; \
        int		isDynObjMethod = 0; \
	int		excpFlag; \
	if (bError)   \
 		*bError = TRUE



private	imeth	int	pSetupJNIMethodCall(char *methodname, char *methodsig, jmethodID *mid, BOOL *isStatic,
					    int *lastparm, jvalue *nParms,
					    int *nParmIntCnt, object *dynDisposeParms,
					    int *dynDisposeParmCnt, jstring *jsParm, int *stringParmCnt)
{
	int *pParms = lastparm + 1;   
	JNIEnv *env = (JNIEnv *)gGetJavaEnv(Java);   
	char *pCurPos;   
	if ((gGetJavaClass(self) == gGetJavaObj(self)) &&  strcmp(methodname, "<init>"))   
		*isStatic = TRUE;   
	*mid = gFindJavaMethod(gGetJavaClassObj(self), methodname, methodsig, *isStatic);   
	pCurPos = methodsig + 1;    
	if (!*mid) {
		SetErrorMsg(self, methodname, methodsig, *isStatic, "Method not found", 0);
		return -1;
	}
	if (*methodsig != '(') {
		SetErrorMsg(self, methodname, methodsig, *isStatic, "Invalid method signature", 0);
		return -2;
	}
	if (!strchr(methodsig, ')')) {
		SetErrorMsg(self, methodname, methodsig, *isStatic, "Invalid method signature", 0);
		return -2;
	}
	while (*pCurPos != ')') 
		if (*pCurPos == 'L') {   
			char *pTmp;           
			object objtmp;
			if (!strncmp(pCurPos, "Ljava/lang/String;", 18)) {
				char	*str = (char *) *pParms;
				if (IsObj((object) str))
					str = gStringValue((object) str);
				jsParm[*stringParmCnt] = (*env)->NewStringUTF(env, str);  
				nParms[*nParmIntCnt].l = (jobject)jsParm[*stringParmCnt];  
				*nParmIntCnt += 1; 
				*stringParmCnt += 1; 
			}  
			/*else if (!strncmp(pCurPos, "Lcom/integra/library/Dynace/runtime/DynaceObject;", 8) */
			else if (strstr(pCurPos, "DynaceObject;") && ClassOf((object)*pParms) != JavaObject)  { 
				objtmp = gAttachDynObj(Java, (object)*pParms);
				if (!IsObj(objtmp)) {
					char	buf[128];
					sprintf(buf, "Argument number %d invalid", 1 + *nParmIntCnt);
					SetErrorMsg(self, methodname, methodsig, *isStatic, buf, 0);
					return -3;
				}
				dynDisposeParms[*dynDisposeParmCnt] = objtmp; 
                                nParms[*nParmIntCnt].l = (jobject)gGetJavaObj(objtmp);
				*nParmIntCnt += 1; 
				*dynDisposeParmCnt += 1; 
			}  else  {  
				objtmp = (object)*pParms;   
				if (!IsObj(objtmp)) {
					char	buf[128];
					sprintf(buf, "Argument number %d invalid", 1 + *nParmIntCnt);
					SetErrorMsg(self, methodname, methodsig, *isStatic, buf, 0);
					return -3;
				}
				nParms[*nParmIntCnt].l = (jobject)gGetJavaObj(objtmp);   
				*nParmIntCnt += 1;
			}  
			pParms++;               
			pTmp = strchr(pCurPos, ';');   
			if (!pTmp)   
				return -2;  
			pCurPos = pTmp + 1;   
		} else if (*pCurPos == 'D' || *pCurPos == 'F') {   
			nParms[*nParmIntCnt].d = *(double *)pParms; 
			pParms += sizeof(double) / sizeof(int);   
			*nParmIntCnt += 1;   
			pCurPos++;    
		} else {           
			nParms[*nParmIntCnt].i = *pParms++; 
			*nParmIntCnt += 1;   
			pCurPos++;    
		}
	return 0;
}


#define CleanupJNIMethodCall() \
	for (nParmIntCnt=0; nParmIntCnt < stringParmCnt; nParmIntCnt++) \
		(*env)->DeleteLocalRef(env, jsParm[nParmIntCnt]); \
	for (nParmIntCnt=0; nParmIntCnt < dynDisposeParmCnt; nParmIntCnt++) \
                gDispose(dynDisposeParms[nParmIntCnt])

private	imeth	setupInstanceVars(jclass jcls, char *className, int DelLocalRef)
{
	JNIEnv	*env = (JNIEnv *)gGetJavaEnv(Java);
	iClass = (*env)->NewGlobalRef(env, jcls);
	if (DelLocalRef)
		(*env)->DeleteLocalRef(env, jcls);
	iMethodIDMap = gNew(StringDictionary);
	iStaticMethodIDMap = gNew(StringDictionary);
	iJavaClassName = gNewWithStr(String, className);
	return self;
}

private	imeth	int	isTwinClass(char *clsname)
{
	int	ret = 0;
	char	cls[256], *pos;

	strcpy(cls, clsname);
	for (pos=cls ; *pos ; pos++)
		if (*pos == '/')
			*pos = '.';
	if (gFindStr(cTwinClassMap, cls))
		ret = 1;
	return ret;
}

cmeth	void	gAddJavaTwinGeneric(int dynTwinClass, char *funcname, void *generic, char *sig, int isStatic)
{
	//this method should only be called from java
	//in a static block that runs once when the java class loads

	//see if method has already been setup
	
	object	objTwin;
	byte *funccall, *funcpos;
	if (!strcmp(funcname, "gDispose")) {
		/* java implementations of gDispose are called in the JavaTwinClass_im_gDispose
		   function which is added in gAddJavaTwinClass
		*/
		return;
	}
	if ((objTwin = gFindValueStr(cJavaTwinGenericMap, funcname)) == NULL) {
		/*  function call memory block layout
		    4 byte storage for caller return address save (funccall)
		    4 byte storage for stack position save (funccall + 4)
		    4 byte storage for JavaCall method error flag (funccall + 8)
		    Java method name as null-term string, "i" + generic name (funccall + 12)
		    Java method signature as null-term string (funccall + 12 + funcNameLen)
		    4 byte ptr to GetIVptr function (funcpos - 8)
		    4 byte ptr to vCallJavaMethod or vCallJavaDoubleMethod (funcpos - 4)
		    assembly code entry point (asmblock) (funcpos or (funccall + 12 + funcNameLen + sigLen + 8))
		*/
		static int	addrSavePos = 3;
		static int	stackSavePos = 9;
		static int	ivPtrPos = 22;
		static int	errPos = 30;
		static int	methsigPos = 35;
		static int	methnamePos = 40;
		static int	callMethPos = 48;
		static int	stackSavePos2 = 54;
		static int	addrSavePos2 = 60;
		int	isDoubleMethod = 0;
		int	funcNameLen = strlen(funcname) + 2,
			sigLen = strlen(sig) + 1;
		if (*(sig + sigLen - 2) == 'D' || *(sig + sigLen - 2) == 'F')
			isDoubleMethod = 1;
		funccall = malloc(sizeof(asmblock) +
				  funcNameLen +
				  sigLen + (5 * sizeof(int)));
		strcpy(funccall + 12, "i");
		strcpy(funccall + 13, funcname);
		strcpy(funccall + 12 + funcNameLen, sig);
		funcpos = funccall + 12 + funcNameLen + sigLen + 8;
		*((int *)(funcpos - 8)) = (int)GetIVptr;
		if (isDoubleMethod)
			*((int *)(funcpos - 4)) = (int)vCallJavaDoubleMethod;
		else
			*((int *)(funcpos - 4)) = (int)vCallJavaMethod;
		memcpy(funcpos, asmblock, sizeof(asmblock));
		*((int *)(funcpos + addrSavePos)) = (int)funccall;
		*((int *)(funcpos + stackSavePos)) = (int)funccall + 4;
		*((int *)(funcpos + errPos)) = (int)funccall + 8;
		*((int *)(funcpos + methnamePos)) = (int)funccall + 12;
		*((int *)(funcpos + methsigPos)) = (int)funccall + 12 + funcNameLen;
		*((int *)(funcpos + ivPtrPos)) = (int)(funcpos - 8);
		*((int *)(funcpos + callMethPos)) = (int)(funcpos - 4);
		*((int *)(funcpos + stackSavePos2)) = (int)funccall + 4;
		*((int *)(funcpos + addrSavePos2)) = (int)funccall;
		gAddStr(cJavaTwinGenericMap, funcname, gNewWithPtr(Pointer, funcpos));
	} else
		funcpos = gPointerValue(objTwin);

	if (isStatic)
		gNewMethod(Method, funcname, ClassOf((object)dynTwinClass), (object)generic,
			   (void *)funcpos, (void *)funcpos);
	else
		gNewMethod(Method, funcname, (object)dynTwinClass, (object)generic,
			   (void *)funcpos, (void *)funcpos);
}

cmeth	gAddJavaTwinClass(int javaclass, char *className, int SuperClassObjectArray, int superCount)
{
	//this method should only be called from java
	//in a static block that runs once when the java class loads
	
	JNIEnv	*env = (JNIEnv *)gGetJavaEnv(Java);
	object	objTwin, obj, cls, clsVar = Class;
	jstring js;
	int	*pcls, ndx, stackFix = 20 + superCount * 4;
	void	*superClass[64];
	void	*superPtr = (void *)&superClass[superCount-1];
	int	isDerivedFromCoreDynClass = 0;

	if (superCount > 63 || !superCount)
		return NULL;
	
	if ((objTwin = gFindValueStr(cTwinClassMap, className)) != NULL)
		return objTwin;
	
	obj = gNew(super);
	//  Subclasses of this class don't use these instance variables.
	//  They redirect to a delagated parent.
	if (self == CLASS)
		setupInstanceVars(obj, (jclass)javaclass, className, 0);
	for (ndx = 0; ndx < superCount; ndx++) {
		char 	*supername;
		jobject	elem;
		
		elem = (jstring)(*env)->GetObjectArrayElement(env, (jobject)SuperClassObjectArray, (jsize)ndx);
		supername = (char *)(*env)->GetStringUTFChars(env, elem, NULL);
		superClass[ndx] = gFindClass(Class, supername);
		if (!isDerivedFromCoreDynClass && !isTwinClass(self, supername))
			isDerivedFromCoreDynClass = 1;
		(*env)->ReleaseStringUTFChars(env, (jstring)elem, (char *)supername);
		(*env)->DeleteLocalRef(env, elem);
		if (!superClass[ndx])
			return NULL;
	}
	_asm  {
			push	0
			mov	ecx, superCount
		next:
			mov	edx, superPtr
			push	[edx]
			sub	ecx, 1
			cmp	ecx, 0
			je	done
			sub	superPtr, 4
			jmp	next
		done:
			push	4
			push	4
			push	className
			push	clsVar
			call	gNewClass
			add	esp, stackFix
			mov	cls, eax
	}
	
	
	pcls = (int *)GetIVptr(cls, ClassOf(cls));
	*pcls = (int)obj;
	js = (*env)->NewStringUTF(env, className);
	(*env)->CallStaticVoidMethod(env, (jclass)gGetDynaceClassClass(Java),
				     (jmethodID)gAddClassMethodID(Java), 
				     js, (int)cls);
	gAddStr(cTwinClassMap, className, cls);

	if (isDerivedFromCoreDynClass) {
		byte	*funccall = malloc(sizeof(gDispose_asmblock));
		memcpy(funccall, gDispose_asmblock, sizeof(gDispose_asmblock));
		*((int *)(funccall + 8)) = (int)cls;
		*((int *)(funccall + 13)) = (int)JavaTwinClass_im_gDisposeWithCls;
		gNewMethod(Method, "gDispose", cls, gDispose_g,
		   (void *)funccall, (void *)funccall);
	}
	return cls;
}


cmeth	gNewWithStr, gFindClass(char *className)
{
	jclass	jcls;
	JNIEnv	*env;
	object	objTwin, obj;
	char	classBuf[256];
	jthrowable exc;

	if (gNewJavaVM(Java))
		return NULL;
	FixClassName(className, classBuf);
	if (obj = gFindValueStr(cJavaClassMap, classBuf))
		return obj;
	//look for class in twin map,  return dynace class of twin if found
	if ((objTwin = gFindValueStr(cTwinClassMap, classBuf)) != NULL)
		return objTwin;
	
	env = (JNIEnv *)gGetJavaEnv(Java);
	if (!env)
		return NULL;
	
	jcls = (*env)->FindClass(env, classBuf);
	if (exc=(*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		gLogError(Java, (void *) exc, JAVAERRORLOG, "");
	}
	if (!jcls) {
		gSetLastJavaError(Java, vBuild(String, "Class Not Found-", className, END));
		return NULL;
	}
#if 0  // What?!?
	//check again in twin map,  return dynace class of twin if found
	if ((objTwin = gFindValueStr(cTwinClassMap, classBuf)) != NULL) {
		(*env)->DeleteLocalRef(env, jcls);
		return objTwin;
	}
#endif
	obj = gNew(super);
	setupInstanceVars(obj, jcls, className, 1);
	gAddStr(cJavaClassMap, classBuf, obj);
	return obj;
}

imeth	gGetJavaClassStaticMethodMap()
{
	return iStaticMethodIDMap;
}

imeth	gGetJavaClassMethodMap()
{
	return iMethodIDMap;
}

private imeth	jmethodID	gFindJavaMethod(char *methodName, char *methodSig, BOOL isStatic)
{
	char	sigbuf[256], objkey[256];
	object	val;
	object	map;
	void	*vpRet = NULL;
	jthrowable exc;

	sprintf(objkey, "%s@%s", FixClassName(methodSig, sigbuf), methodName);
	if (!gGetJavaClass(self))
		return NULL;

	if (isStatic && !strcmp(methodName, "<init>"))
		return NULL;

	map = isStatic ? iStaticMethodIDMap : iMethodIDMap;

	val = gFindValueStr(map, objkey);
	if (!val) {
		JNIEnv *env = (JNIEnv *)gGetJavaEnv(Java);
		jmethodID mid;

		if (isStatic)
			mid = (*env)->GetStaticMethodID(env, gGetJavaClass(self), methodName, sigbuf);
		else
			mid = (*env)->GetMethodID(env, gGetJavaClass(self), methodName, sigbuf);
		vpRet = (void *)mid;
		if (exc=(*env)->ExceptionOccurred(env)) {
			(*env)->ExceptionClear(env);
			gLogError(Java, (void *) exc, JAVAERRORLOG, "");
		}
		if (mid) {
			object vptr = gNewWithPtr(Pointer, vpRet);
			gAddStr(map, objkey, vptr);
		}
	} else
		vpRet = gPointerValue(val);

	return vpRet;
}

imeth	char	*gGetJavaClassName()
{
	return gStringValue(iJavaClassName);
}
 
imeth	void	*gGetJavaObj()
{
	return (void *)iClass;
}

imeth	void	*gGetJavaClass()
{
	return gGetJavaObj(self);
}

cmeth	gAttachJavaObject(char *ClassName, void *javaObj)
{
	object	jcls = gNewWithStr(JavaClass, ClassName), objRet;
	int	twinClassFlag = 0;
	/*  check for dynace twin object */
	if (ClassOf(jcls) != JavaClass)	{
		/* class returned is a dynace twin class */
		/* get the underlying JavaClass instance */
		void *cv = GetIVptr(jcls, ClassOf(jcls));
		jcls = *((object *)cv);
		twinClassFlag = 1;
	}
	objRet = gNewWithJavaObj(JavaObject, jcls, (jobject)javaObj);
	if (!twinClassFlag)
		gSetDisposeParent(objRet, TRUE);
	return objRet;
}

cvmeth	vNewJavaObject(char *ClassName, char *ctorSig, ...) 
{
	object	jcls = gNewWithStr(JavaClass, ClassName), objret;
	BOOL	*bError = NULL;
	SetupJNIMethodCallLocals();
	if (!jcls)
		return NULL;
	returnval = pSetupJNIMethodCall(jcls, "<init>", ctorSig, &mid, &isStatic, (int *)&ctorSig, nParms,
						&nParmIntCnt, dynDisposeParms, &dynDisposeParmCnt,
						jsParm, &stringParmCnt);
	if (returnval)
		return NULL;
	returnval = pCallMethod(jcls, env, (*env)->NewObjectA, mid, gGetJavaClass(jcls), nParms, nParmIntCnt, &excpFlag);
	CleanupJNIMethodCall();
	if (excpFlag) {
		object	msg = vBuild(String, "Java exception creating a new instance of ", ClassName, " ", ctorSig, END);
		gSetLastJavaError(Java, msg);
	}
	if (!returnval)
		return NULL;
	objret = gNewWithJavaObj(JavaObject, jcls, (void *)returnval);
	gSetDisposeParent(objret, TRUE);
	return objret;
}

ivmeth	vNewJavaObject(char *methodSig, ...) 
{
	BOOL	*bError = NULL;
	SetupJNIMethodCallLocals();
	returnval = pSetupJNIMethodCall(self, "<init>", methodSig, &mid, &isStatic, (int *)&methodSig, nParms,
						&nParmIntCnt, dynDisposeParms, &dynDisposeParmCnt,
						jsParm, &stringParmCnt);
	if (returnval)
		return NULL;
	returnval = pCallMethod(self, env, (*env)->NewObjectA, mid, gGetJavaClass(self), nParms, nParmIntCnt, &excpFlag);
	CleanupJNIMethodCall();
	if (excpFlag) {
		object msg = vBuild(String, "Java exception creating a new instance ", methodSig, END);
		gSetLastJavaError(Java, msg);
	}
	if (!returnval)
		return NULL;
	return gNewWithJavaObj(JavaObject, self, (void *)returnval);
}

ivmeth	int 	vCallJavaMethod(char *methodName, char *methodSig, BOOL *bError, ...) 
{
	SetupJNIMethodCallLocals();
	returnval = pSetupJNIMethodCall(self, methodName, methodSig, &mid, &isStatic, (int *)&bError, nParms,
						&nParmIntCnt, dynDisposeParms, &dynDisposeParmCnt,
						jsParm, &stringParmCnt);
	if (returnval)
		return returnval;
	pGetJNIFunction(self, methodSig, isStatic, &vpFunc, &isStringMethod, &isObjectMethod, &isDynObjMethod);
	returnval = pCallMethod(self, env, vpFunc, mid, gGetJavaObj(self), nParms, nParmIntCnt, &excpFlag);
	CleanupJNIMethodCall();
	if (excpFlag) {
		*bError = 1;
		SetErrorMsg(self, methodName, methodSig, isStatic, "exception", 1);
		return 0;
	}
	if (isStringMethod) {
		jstring js = (jstring)returnval;
		const char *str;
		if (!js)
			return 0;
		str = (*env)->GetStringUTFChars(env, js, NULL);
		returnval = (int)gNewWithStr(String, (char *)str);
		(*env)->ReleaseStringUTFChars(env, js, str);
		(*env)->DeleteLocalRef(env, js);
	}
	if (isObjectMethod) {
		jclass jc = (*env)->GetObjectClass(env, (jobject)returnval);
		if (jc)	{
			jboolean jb = (*env)->IsSameObject(env, jc, (void *)gGetBaseObjectClass(Java));
			if (returnval)
				if (jb == JNI_TRUE)
					isDynObjMethod = 1;
				else {
					char	returnClass[256];
					object	cls;
					ExtractClassName(methodSig, returnClass);
					cls = gNewWithStr(CLASS, returnClass);
					returnval = (int)gNewWithJavaObj(JavaObject, cls, (void *)returnval);
				}
			(*env)->DeleteLocalRef(env, jc);
		}
	}
	if (isDynObjMethod  &&  returnval) {
		jobject	jobj = (jobject)returnval;
		returnval = gGetAddress(Java, (object)jobj);
		(*env)->DeleteLocalRef(env, jobj);
	}
	*bError = FALSE;
	return returnval;
}

ivmeth	double 	vCallJavaDoubleMethod(char *methodName, char *methodSig, BOOL *bError, ...) 
{
	double dblret;
	SetupJNIMethodCallLocals();
	returnval = pSetupJNIMethodCall(self, methodName, methodSig, &mid, &isStatic, (int *)&bError, nParms,
						&nParmIntCnt, dynDisposeParms, &dynDisposeParmCnt,
						jsParm, &stringParmCnt);
	if (returnval)
		return (double)returnval;
	pGetJNIFunction(self, methodSig, isStatic, &vpFunc, &isStringMethod, &isObjectMethod, &isDynObjMethod);
	dblret = pCallDoubleMethod(self, env, vpFunc, mid, gGetJavaObj(self), nParms, nParmIntCnt, &excpFlag);
	CleanupJNIMethodCall();
	if (excpFlag) {
		*bError = 1;
		SetErrorMsg(self, methodName, methodSig, isStatic, "exception", 1);
		return 0;
	}
	*bError = FALSE;
	return dblret;
}

private imeth	int	pCallMethod(JNIEnv *env, void *fp, jmethodID mid, void *javaobj, jvalue *nParms, 
							int nParmIntCnt, int *excpFlag) 
{
	int	returnval;
	jthrowable exc;

	*excpFlag = 0;
	returnval = (*(int (JNICALL *)())  fp)(env, javaobj, mid, nParms);
	if (exc=(*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		gLogError(Java, (void *) exc, JAVAERRORLOG, "");
		*excpFlag = 1;
	}
	return returnval;
}

private imeth	double	pCallDoubleMethod(JNIEnv *env, void *fp, jmethodID mid, void *javaobj, jvalue *nParms, 
							int nParmIntCnt, int *excpFlag) 
{
	double dRet;
	jthrowable exc;

	*excpFlag = 0;
	dRet = (*(double (JNICALL *)())  fp)(env, javaobj, mid, nParms);
	if (exc=(*env)->ExceptionOccurred(env)) {
		(*env)->ExceptionClear(env);
		gLogError(Java, (void *) exc, JAVAERRORLOG, "");
		*excpFlag = 1;
	}

	return dRet;
}

imeth	gDispose, gDeepDispose ()
{
	JNIEnv *env;
	env = (JNIEnv *)gGetJavaEnv(Java);
	if (env && iClass)
		(*env)->DeleteGlobalRef(env, iClass);
	if (iMethodIDMap)
		gDeepDispose(iMethodIDMap);
	if (iStaticMethodIDMap)
		gDeepDispose(iStaticMethodIDMap);
	if (iJavaClassName) {
		char	*name = gStringValue(iJavaClassName);
		gRemoveStr(cJavaClassMap, name);
		gRemoveStr(cTwinClassMap, name);
		gDispose(iJavaClassName);
	}
	return gDispose(super);
}

static	object	JavaTwinClass_im_gDisposeWithCls(object cls, object self)
{
	object	dynClassObj = ClassOf(self), objRet;
	object	*iv = GetIVptr(self, dynClassObj), javaObj = *iv;
	BOOL	bErr;

	vCallJavaMethod(javaObj, "igDispose", "()L" PACKAGE(DynaceBase) ";", &bErr);
	objRet = oSuper(cls, gDispose, self)(self);
	gDispose(javaObj);
	return objRet;
}

imeth	gGetJavaClassObj()
{
	return self;
}

static	void	class_init(void)
{
	cJavaClassMap = gNew(StringDictionary);
	cTwinClassMap = gNew(StringDictionary);
	cJavaTwinGenericMap = gNew(StringDictionary);
}
