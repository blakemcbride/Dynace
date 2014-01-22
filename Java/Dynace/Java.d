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
#define itoa _itoa
#endif
#endif

#include "jni.h"
#include "stdlib.h"

extern	void	LoadDynaceClasses();
static	char	*StringSlashFix(char *buf);

defclass  Java  {
 class:
	object		cJVMPath;
	JavaVM		*cJavaJVM;
	jmethodID	cAttachDynaceObjMethodID;
	jmethodID	cGetAddressMethodID;
	object 		cDynaceObjClass;
	object		cLastErrorMsg;
	object		cObjectPackageName;
	jclass		cDynaceClassClass;
	jmethodID	cAddClassMethodID;
	int		cDebugJVMPort;
	object		cClassPath;
	object		cLibraryPath;
	ifun		cInitCallback;
	object		cJavaPropList;
	int		cJavaDebugSuspend;
	/* cJavaObjectCls is typed as object but is really a jclass */
	object		cJavaObjectCls;
};

#include "package.h"

static	object	fixPackageDelim(char *clsname)
{
	object	ret = vBuild(String, clsname, "/DynaceObject", END);
	char *pos = gStringValue(ret);;
	for (; *pos ; pos++)
		if (*pos == '.')
			*pos = '/';
	return ret;
}

cmeth	int	gGetDynaceClassClass()
{
	return (int)cDynaceClassClass;
}

cmeth	int	gAddClassMethodID()
{
	return (int)cAddClassMethodID;
}

cmeth	char	*gGetObjectPackageClassName()
{
	return gStringValue(cObjectPackageName);
}

cmeth	gGetBaseObjectClass()
{
	return cJavaObjectCls;
}

cmeth	char	*gGetLastJavaError()
{
	return cLastErrorMsg ? gStringValue(cLastErrorMsg) : "";
}

cmeth	void	gSetLastJavaError(object message)
{
	if (cLastErrorMsg)
		gDispose(cLastErrorMsg);

	cLastErrorMsg = message;
}

cmeth	void	*gGetJavaEnv() 
{
	JNIEnv	*env;
	(*cJavaJVM)->AttachCurrentThread(cJavaJVM, (void **)&env, NULL);
	return (void *)env;
}

cmeth	void	*gGetJavaVM() 
{
	return (void *)cJavaJVM;
}

cmeth	void	gDetachCurrentThreadFromVM()
{
	(*cJavaJVM)->DetachCurrentThread(cJavaJVM);
}

cmeth	gAttachDynObj(object obj) 
{
	JNIEnv	*env = gGetJavaEnv(Java);
	return gNewWithJavaObj(JavaObject, cDynaceObjClass,
			(void *)(*env)->CallStaticObjectMethod(env, 
		(jclass)gGetJavaObj(cDynaceObjClass), 
		cAttachDynaceObjMethodID, ClassOf(obj), obj));
}

cmeth	void	gAttachJavaObj(int javaobj, int dynobj)
{
	int	*pobj = (int *)GetIVptr((object) dynobj, ClassOf(dynobj));
	object	*pcls = (object *)GetIVptr(ClassOf((object)dynobj), ClassOf(ClassOf((object)dynobj)));
	*pobj = (int)gNewWithJavaObj(JavaObject, *pcls, (void *)javaobj);
}

cmeth	int	gGetAddress(object obj) 
{
	JNIEnv	*env = gGetJavaEnv(Java);
	return (*env)->CallIntMethod(env, 
		(jobject)obj, 
		cGetAddressMethodID);
}

cmeth	void	gSetJavaProperty(char *propStr)
{
	if (!propStr)
		return;
	if (!cJavaPropList)
		cJavaPropList = gNew(LinkObject);
	gAddLast(cJavaPropList, gNewWithStr(String, propStr));
}

cmeth	void	gSetJavaInitParameters(char *clsPath, char *libPath, char *objectPackageName, ifun initCallback)
{
	JavaCallbackClassSurrogate = JavaCallback;
	if (clsPath && *clsPath != '\0')
		cClassPath = gNewWithStr(String, clsPath);
	if (libPath && *libPath != '\0')
		cLibraryPath = gNewWithStr(String, libPath);
	if (objectPackageName && *objectPackageName != '\0')
		cObjectPackageName = fixPackageDelim(objectPackageName);
	else
		cObjectPackageName = gNewWithStr(String, "DynaceObject");
	if (initCallback)
		cInitCallback = initCallback;
}

cmeth	void	gSetJavaDebugPort(int debugPort)
{
	cDebugJVMPort = debugPort;
}

cmeth	void	gSetJavaDebugSuspend()
{
	cJavaDebugSuspend = 1;
}

cmeth	void	gSetJavaVM(void *javaEnv, char *objectPackageName, ifun initCallback)
{
	JavaCallbackClassSurrogate = JavaCallback;
	cJavaJVM = (JavaVM *)javaEnv;
	if (objectPackageName && *objectPackageName != '\0')
		cObjectPackageName = fixPackageDelim(objectPackageName);
	else
		cObjectPackageName = gNewWithStr(String, "DynaceObject");
	if (initCallback)
		cInitCallback = initCallback;
	gNewJavaVM(Java);
}

cmeth	int	gNewJavaVM() 
{
	JNIEnv		*env;

	if (!cJavaJVM) {
		object		clsPath, libPath, bootCP = NULL, jvmDll;
		FARPROC		loadVMFunc;
		JavaVMOption 	options[128];
		char		jvmPath[256];
		JavaVMInitArgs 	vm_args;
		jint 		res;
		char 		DbgOpt[64];
		BOOL		debug = FALSE;
		char		*systemClassPath;
		int		vmOptIndex = 4;

		if (!cClassPath) {
			gSetLastJavaError(Java, gNewWithStr(String, "Invalid Java Init Parameters, ClassPath = NULL"));
			return 1;
		}
		if (!cLibraryPath) {
			gSetLastJavaError(Java, gNewWithStr(String, "Invalid Java Init Parameters, LibPath = NULL"));
			return 1;
		}

		/* load the JVM  */
		if (cJVMPath)
			sprintf(jvmPath, "%s\\jvm.dll", gStringValue(cJVMPath));
		else
			strcpy(jvmPath, "jvm.dll");
		jvmDll = gLoadLibrary(DynamicLibrary, jvmPath);
		if (!jvmDll) {
			gSetLastJavaError(Java, vBuild(String, "; ", jvmPath, " not found", NULL));
			return 1;
		}

		systemClassPath = getenv("CLASSPATH");
		if (systemClassPath && *systemClassPath != '\0') {
			char	buff[256];
			strcpy(buff, systemClassPath);
			clsPath = vBuild(String, "-Djava.class.path=", StringSlashFix(gStringValue(cClassPath)), ";", StringSlashFix(buff), END);
		} else
			clsPath = vBuild(String, "-Djava.class.path=", StringSlashFix(gStringValue(cClassPath)), END);
		libPath = vBuild(String, "-Djava.library.path=", gStringValue(cLibraryPath), END);
		
		options[0].optionString = gStringValue(clsPath);
		options[1].optionString = "-verbose:jni";
		options[2].optionString = gStringValue(libPath);
		options[3].optionString = "-Xcheck:jni";
		if (cJavaPropList) {
			object	propSeq, propstr;
			for (propSeq=gSequence(cJavaPropList) ; propstr = gNext(propSeq) ; ) 
				options[vmOptIndex++].optionString = gStringValue(propstr);
		}
		if (cDebugJVMPort) {
			char	*javaHome = getenv("JAVA_HOME"), debugSuspend[2];
			if (!javaHome || *javaHome == '\0') {
				gSetLastJavaError(Java, vBuild(String, "Debug port specified but JAVA_HOME",
							       " environment variable is missing or invalid",
							       END));
				return 1;
			}
			if (cJavaDebugSuspend)
				strcpy (debugSuspend, "y");
			else
				strcpy (debugSuspend, "n");
			sprintf(DbgOpt, "-Xrunjdwp:transport=dt_socket,address=%d,server=y,suspend=%s", cDebugJVMPort, debugSuspend);
			bootCP = vBuild(String, "-Xbootclasspath:", javaHome, "/lib/tools.jar;"
					, javaHome, "/jre/lib/rt.jar", END);
			options[vmOptIndex++].optionString = "-Xdebug";
			options[vmOptIndex++].optionString = DbgOpt;
			//options[vmOptIndex++].optionString = gStringValue(bootCP);
			options[vmOptIndex++].optionString = "-Xnoagent";
			debug = TRUE;
		}

		vm_args.version = JNI_VERSION_1_2;
		vm_args.options = options;
		vm_args.nOptions = vmOptIndex;
		vm_args.ignoreUnrecognized = TRUE;

		loadVMFunc = gGetProcAddress(jvmDll, "JNI_CreateJavaVM");
		if (!loadVMFunc) {
			gSetLastJavaError(Java, vBuild(String, "Error creating Java VM-JNI_CreateJavaVM not found", END));
			return -1;
		}
		res = loadVMFunc(&cJavaJVM, (void **)&env,(void *)&vm_args);
		if (res < 0) {
			char msgbuf[24];
			gSetLastJavaError(Java, vBuild(String, "Error creating Java VM-", itoa(res, msgbuf, 10), END));
			return res;
		}
		if (bootCP)
			gDispose(bootCP);
		gDispose(clsPath);
		gDispose(libPath);
	}  else
		env = gGetJavaEnv(Java);
	if (!cJavaObjectCls) {
		jclass 		clsDynaceGeneric = NULL, clsDynObj = NULL, objClassLocal;
		jmethodID 	mid_addGeneric = NULL, mid_setupGenericAddresses=NULL;
		object		d_cls = NULL, seq = NULL, objAss = NULL;
		jclass		dynaceCls = NULL;
		jstring 	js = NULL;

//		Scheme;
		objClassLocal = (*env)->FindClass(env, gStringValue(cObjectPackageName));
		if (!objClassLocal) {
			gSetLastJavaError(Java, vBuild(String, "Class Load Error for class DynaceObject. ", 
						       "Verify that the class DynaceObject ",
						       "is on the classpath ", gStringValue(cClassPath), END)); 
			return 1;
		}
		cJavaObjectCls = (object)(*env)->NewGlobalRef(env, objClassLocal);
		(*env)->DeleteLocalRef(env, objClassLocal);
		
		dynaceCls = (*env)->FindClass(env, PACKAGE(DynaceClass));
		if (dynaceCls == 0) {
			gSetLastJavaError(Java, vBuild(String, "Class Load Error for class DynaceClass. ", 
						       "Verify that the class DynaceClass ",
						       "is on the classpath ", gStringValue(cClassPath),
						       " and the file JavaDynace.dll is on ",
						       "the path ", gStringValue(cLibraryPath), END)); 
			return 1;
		}
		cDynaceClassClass = (*env)->NewGlobalRef(env, dynaceCls);
		(*env)->DeleteLocalRef(env, dynaceCls);
 
		cDynaceObjClass = gNewWithStr(JavaClass, PACKAGE(DynaceBase));
		if (cDynaceObjClass == 0) {
			gSetLastJavaError(Java, vBuild(String, "Can't find class DynaceBase,classpath=",
						       gStringValue(cClassPath), END));
			return 1;
		}

		clsDynaceGeneric = (*env)->FindClass(env, PACKAGE(DynaceGeneric));
		if (clsDynaceGeneric == 0) {
			gSetLastJavaError(Java, vBuild(String, "Can't find class DynaceGeneric,classpath=",
						       gStringValue(cClassPath), END));
			return 1;
		}

		cAddClassMethodID = (*env)->GetStaticMethodID(env, cDynaceClassClass, "addClass", 
								   "(Ljava/lang/String;I)V");
		if (!cAddClassMethodID) {
			gSetLastJavaError(Java, vBuild(String, "Can't find method DynaceClass::addClass", END));
			return 1;
		}
		
		clsDynObj = (jclass)gGetJavaObj(cDynaceObjClass);
		cAttachDynaceObjMethodID = (*env)->GetStaticMethodID(env, clsDynObj, 
									  "attachDynaceBase", 
									  "(II)L" PACKAGE(DynaceBase) ";");
		if (!cAttachDynaceObjMethodID) {
			gSetLastJavaError(Java, vBuild(String, "Can't find method DynaceObject::attachDynaceBase", END));
			return 1;
		}
		cGetAddressMethodID = (*env)->GetMethodID(env, clsDynObj, 
							       "getAddress", 
							       "()I");
		if (!cGetAddressMethodID) {
			gSetLastJavaError(Java, vBuild(String, "Can't find method DynaceObject::getAddress", END));
			return 1;
		}

		mid_addGeneric = (*env)->GetStaticMethodID(env, clsDynaceGeneric, 
								"addGeneric", 
								"(Ljava/lang/String;II)V");
		if (!mid_addGeneric) {
			gSetLastJavaError(Java, vBuild(String, "Can't find method DynaceGeneric::addGeneric", END));
			return 1;
		}

		LoadDynaceClasses();
		d_cls = gGetAll(Class);
		for (seq = gSequence(d_cls)  ;  objAss = gNext(seq) ; ) {
			js = (*env)->NewStringUTF(env, gName(objAss));
			(*env)->CallStaticVoidMethod(env, cDynaceClassClass, cAddClassMethodID, 
							  js, (int)objAss);
			(*env)->DeleteLocalRef(env, js);
		}
		gDispose(d_cls);

		d_cls = gGetAll(GenericFunction);
		for (seq = gSequence(d_cls)  ;  objAss = gNext(seq) ; ) {
			js = (*env)->NewStringUTF(env, gName(objAss));
			(*env)->CallStaticVoidMethod(env, clsDynaceGeneric, mid_addGeneric, 
							  js, (int)gGetGenericPtr(objAss), (int)objAss);
			(*env)->DeleteLocalRef(env, js);
		}
		js = (*env)->NewStringUTF(env, "DynaceSuperCall");
		(*env)->CallStaticVoidMethod(env, clsDynaceGeneric, mid_addGeneric, 
						  js, (int)_FindMethod2, 0);

		(*env)->DeleteLocalRef(env, js);
		gDispose(d_cls);

		mid_setupGenericAddresses = (*env)->GetStaticMethodID(env, (jclass)cJavaObjectCls, 
								"setupGenericAddresses", 
								"()V");
		if (!mid_setupGenericAddresses) {
			gSetLastJavaError(Java, vBuild(String, "Can't find method DynaceGeneric::setupGenericAddresses", END));
			return 1;
		}
		(*env)->CallStaticVoidMethod(env, (jclass)cJavaObjectCls, mid_setupGenericAddresses); 
		
		JavaCallbackClassSurrogate = JavaCallback;
		if (cInitCallback && !cInitCallback())
			return -1;
	}

	return 0;
}

static	char	*StringSlashFix(char *buf)
{
	char	*pos;
	for (pos=buf ; *pos ; pos++)
		if (*pos == '/')
			*pos = '\\';
	return buf;
}

cmeth	gSetJVMPath(char *path)
{
	if (cJVMPath)
		cJVMPath = gDispose(cJVMPath);
	if (path)
		cJVMPath = gNewWithStr(String, path);
	return self;
}

cmeth	void	gLogError(void *exc, char *fileName, char *msg)
{
	BOOL	err;
	object	eo = gNewWithJavaObj(JavaObject, NULL, exc);

	vCallJavaMethod(cDynaceObjClass, "logError", "(Ljava/lang/Throwable;Ljava/lang/String;Ljava/lang/String;)V", &err, eo, fileName, msg);
	gDispose(eo);
//	if (err)
//		gError(self, gGetLastJavaError(Java));
}
