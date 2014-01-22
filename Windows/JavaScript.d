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

#define	XP_WIN

#include <io.h>
#include "jsapi.h"

    /*
     * Tune this to avoid wasting space for shallow stacks, while saving on
     * malloc overhead/fragmentation for deep or highly-variable stacks.
     */
#define STACK_CHUNK_SIZE    8192

#define ERRORMSG_PROP "ErrorMsg"

defclass JavaScript {
	JSContext	*iCX;
	JSObject	*iGlobalObj;
 class:
	JSRuntime	*cRT;
	cGlobalInstance;
 init:  class_init;
};

void JS_DLL_CALLBACK defaultErrorReporter(JSContext* ctx, const char* msg, JSErrorReport* rpt);
static void clearContextError(JSContext* ctx);
static void setContextError(JSContext* ctx, object errmsg);
static JSErrorReporter setErrorReporter(JSContext* ctx, JSErrorReporter rptr, int resetErrors);

extern	void	JavaScript_init_base(void);
extern	void	JavaScript_init_app(void);

static	JSContext *JS_CreateContext(JSRuntime *rt);
static	JSBool  constructor(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval);
static	JSBool	JS_StringToObject(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval);
static	JSBool	JS_StringToPointer(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval);

static	JSContext	*Cache_cx;
static	jsval		DO_class_obj, DO_proto, P_class_obj, P_proto;
static	JSClass		*DO_classp, *P_classp;

static	void	load_context(JSContext *cx)
{
	JS_GetProperty(cx, JS_GetGlobalObject(cx), "DynaceObject", &DO_class_obj);
	JS_GetProperty(cx, (JSObject *)DO_class_obj, "prototype", &DO_proto);
	DO_classp = JS_GetClass((JSObject *)DO_proto);

	JS_GetProperty(cx, JS_GetGlobalObject(cx), "CPointer", &P_class_obj);
	JS_GetProperty(cx, (JSObject *)P_class_obj, "prototype", &P_proto);
	P_classp = JS_GetClass((JSObject *)P_proto);

	Cache_cx = cx;
}

private	imeth	init()
{
	JSClass *dc, *pc;
	static JSClass Dynace_class = { 
		"DynaceObject", JSCLASS_HAS_PRIVATE,
		JS_PropertyStub,JS_PropertyStub,JS_PropertyStub,JS_PropertyStub, 
		JS_EnumerateStub,JS_ResolveStub,JS_ConvertStub,JS_FinalizeStub 
	};
	static JSClass CPointer_class = { 
		"CPointer", JSCLASS_HAS_PRIVATE,
		JS_PropertyStub,JS_PropertyStub,JS_PropertyStub,JS_PropertyStub, 
		JS_EnumerateStub,JS_ResolveStub,JS_ConvertStub,JS_FinalizeStub 
	};
	
	iCX = JS_CreateContext(cRT);
	iGlobalObj = JS_GetGlobalObject(iCX);
	JS_SetContextPrivate(iCX, gNew(PropertyList));
	
	dc = (JSClass *) malloc(sizeof(JSClass));
	memcpy(dc, &Dynace_class, sizeof(JSClass));
	JS_InitClass(iCX, iGlobalObj, 0, dc, constructor, 1, NULL, NULL, NULL, NULL);

	pc = (JSClass *) malloc(sizeof(JSClass));
	memcpy(pc, &CPointer_class, sizeof(JSClass));
	JS_InitClass(iCX, iGlobalObj, 0, pc, constructor, 1, NULL, NULL, NULL, NULL);

	if (!cGlobalInstance)
		cGlobalInstance = self;

	gAddJSGeneric(JavaScript, "StringToObject", JS_StringToObject, 1);
	gAddJSGeneric(JavaScript, "StringToPointer", JS_StringToPointer, 1);
	
	JavaScript_init_base();
	JavaScript_init_app();

	return self;
}

cmeth	gNew()
{
	return init(gNew(super));
}

imeth	gDispose, gDeepDispose()
{
	if (self == cGlobalInstance)
		cGlobalInstance = NULL;
	JS_DestroyContext(iCX);
	return gDispose(super);
}

static	JSContext *JS_CreateContext(JSRuntime *rt)
{
	JSContext *cx;
	JSObject *globalObj;
	static	JSClass global_class = { 
		"global",0, 
		JS_PropertyStub,JS_PropertyStub,JS_PropertyStub,JS_PropertyStub, 
		JS_EnumerateStub,JS_ResolveStub,JS_ConvertStub,JS_FinalizeStub 
	}; 
	JSClass *gc = (JSClass *) malloc(sizeof(JSClass));

	memcpy(gc, &global_class, sizeof(JSClass));

	cx = JS_NewContext(rt, STACK_CHUNK_SIZE);
	if (!cx)
		gError(Dynace, "can't create JavaScript context");
	globalObj = JS_NewObject(cx, gc, 0, 0);
	JS_InitStandardClasses(cx, globalObj);
	return cx;
}

imeth	void	*gNewJSDynaceObject(obj)
{
	jsval	class_obj, proto;
	JSObject *inst;
	JSClass *cp;
	
	JS_GetProperty(iCX, iGlobalObj, "DynaceObject", &class_obj);
	JS_GetProperty(iCX, (JSObject *)class_obj, "prototype", &proto);
	cp = JS_GetClass((JSObject *)proto);
	inst = JS_NewObject(iCX, cp, NULL, NULL);
	JS_SetPrivate(iCX, inst, obj);
	return (void *) inst;
}

cmeth	void	*gNewJSDynaceObject(obj)
{
	return gNewJSDynaceObject(cGlobalInstance, obj);
}

imeth	void	*gNewJSPointer(void *obj)
{
	jsval	class_obj, proto;
	JSObject *inst;
	JSClass *cp;
	
	JS_GetProperty(iCX, iGlobalObj, "CPointer", &class_obj);
	JS_GetProperty(iCX, (JSObject *)class_obj, "prototype", &proto);
	cp = JS_GetClass((JSObject *)proto);
	inst = JS_NewObject(iCX, cp, NULL, NULL);
	JS_SetPrivate(iCX, inst, obj);
	return (void *) inst;
}

cmeth	void	*gNewJSPointer(void *obj)
{
	return gNewJSPointer(cGlobalInstance, obj);
}

imeth	gSetJSClassPropToObj(char *cls, char *prop, obj)
{
	jsval	class_obj, proto;
	JSObject *inst;
	JSClass *cp;
	
	JS_GetProperty(iCX, iGlobalObj, cls, &class_obj);
	JS_GetProperty(iCX, (JSObject *)class_obj, "prototype", &proto);
	cp = JS_GetClass((JSObject *)proto);
	inst = JS_NewObject(iCX, cp, NULL, NULL);
	JS_SetPrivate(iCX, inst, obj);
	JS_SetProperty(iCX, (JSObject *)class_obj, prop, (jsval *)&inst);
	return self;
}

cmeth	gSetJSClassPropToObj(char *cls, char *prop, obj)
{
	gSetJSClassPropToObj(cGlobalInstance, cls, prop, obj);
	return self;
}

imeth	gSetJSClassProp(char *cls, char *prop, long val)
{
	jsval	class_obj;
	JS_GetProperty(iCX, iGlobalObj, cls, &class_obj);
	JS_SetProperty(iCX, (JSObject *)class_obj, prop, &val);
	return self;
}

cmeth	gSetJSClassProp(char *cls, char *prop, long val)
{
	gSetJSClassProp(cGlobalInstance, cls, prop, val);
	return self;
}

imeth	gSetJSInstanceProp(char *cls, char *prop, long val)
{
	jsval	class_obj, proto;
	JS_GetProperty(iCX, iGlobalObj, cls, &class_obj);
	JS_GetProperty(iCX, (JSObject *)class_obj, "prototype", &proto);
	JS_SetProperty(iCX, (JSObject *)proto, prop, &val);
	return self;
}

cmeth	gSetJSInstanceProp(char *cls, char *prop, long val)
{
	gSetJSInstanceProp(cGlobalInstance, cls, prop, val);
	return self;
}

imeth	gAddJSGeneric(char *name, void *gen, int argc)
{
	JS_DefineFunction(iCX, iGlobalObj, name, gen, argc, 0);
	return self;
}

cmeth	gAddJSGeneric(char *name, void *gen, int argc)
{
	gAddJSGeneric(cGlobalInstance, name, gen, argc);
	return self;
}

imeth	gSetJSGlobalToObj(char *name, obj)
{
	jsval	val = (jsval) gNewJSDynaceObject(self, obj);
	JS_SetProperty(iCX, iGlobalObj, name, &val);
	return self;
}

cmeth	gSetJSGlobalToObj(char *name, obj)
{
	gSetJSGlobalToObj(cGlobalInstance, name, obj);
	return self;
}


imeth	int	gExecuteFile(char *file)
{
	jsval rval=0;
	JSErrorReporter oldReporter = setErrorReporter(iCX, defaultErrorReporter, 1);
	JSScript *jscript = JS_CompileFile(iCX, iGlobalObj, file);
	if (jscript) {
		JSBool ret = JS_ExecuteScript(iCX, iGlobalObj, jscript, &rval);
/*
		if (ret == JS_TRUE) {
			printf("success\n");
			if (JSVAL_IS_INT(rval)) {
				int v = JSVAL_TO_INT(rval);
				printf("Return value is %d\n", v);
			}
		} else
			printf("fail");
*/
		JS_DestroyScript(iCX, jscript);
	}
/*
	  else
		printf("fail to load");
*/
	setErrorReporter(iCX, oldReporter, 0);
	return rval;
}

cmeth	int	gExecuteFile(char *file)
{
	return gExecuteFile(cGlobalInstance, file);
}

imeth	gExecuteString(char *script)
{
	jsval rval;
	JSBool ret = JS_FALSE;
	JSErrorReporter oldReporter = setErrorReporter(iCX, defaultErrorReporter, 1);
	
	JSScript *jscript = JS_CompileScript(iCX, iGlobalObj, script, strlen(script), "in memory", 0);
	if (jscript) {
		ret = JS_ExecuteScript(iCX, iGlobalObj, jscript, &rval);
/*
		if (ret == JS_TRUE) {
			printf("success\n");
			if (JSVAL_IS_INT(rval)) {
				int v = JSVAL_TO_INT(rval);
				printf("Return value is %d\n", v);
			}
		} else
			printf("fail");
*/
		JS_DestroyScript(iCX, jscript);
	}
/*
	  else
		printf("fail to compile");
*/
	setErrorReporter(iCX, oldReporter, 0);
	return ret == JS_TRUE ? gJavaScriptToDynace(self, (void *) rval) : NULL;
}

cmeth	gExecuteString(char *script)
{
	return gExecuteString(cGlobalInstance, script);
}

imeth	void	gExecuteStringNR(char *script)
{
	jsval rval;
	JSBool ret = JS_FALSE;
	JSErrorReporter oldReporter = setErrorReporter(iCX, defaultErrorReporter, 1);
	
	JSScript *jscript = JS_CompileScript(iCX, iGlobalObj, script, strlen(script), "in memory", 0);
	if (jscript) {
		ret = JS_ExecuteScript(iCX, iGlobalObj, jscript, &rval);
/*
		if (ret == JS_TRUE) {
			printf("success\n");
			if (JSVAL_IS_INT(rval)) {
				int v = JSVAL_TO_INT(rval);
				printf("Return value is %d\n", v);
			}
		} else
			printf("fail");
*/
		JS_DestroyScript(iCX, jscript);
	}
/*
	  else
		printf("fail to compile");
*/
	setErrorReporter(iCX, oldReporter, 0);
}

cmeth	void	gExecuteStringNR(char *script)
{
	gExecuteStringNR(cGlobalInstance, script);
}

static JSBool  constructor(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval)
{
	*rval = OBJECT_TO_JSVAL(obj);
        return JS_TRUE;
}

static	JSBool	JS_StringToObject(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval)
{
	static	char	fun[] = "StringToObject";
	object	r;

	if (argc != 1)
		vError(Dynace, "JavaScript: %s expects %d arguments, given %d.", fun, 1, argc);

	if (!JSVAL_IS_STRING(argv[0]))
		vError(Dynace, "JavaScript:  %s passed an invalid string in arg 1", fun);
	r = (object) atol(JS_GetStringBytes(JSVAL_TO_STRING(argv[0])));
	*rval = OBJECT_TO_JSVAL(gNewJSDynaceObject(JavaScript, r));
	return JS_TRUE;
}

static	JSBool	JS_StringToPointer(JSContext *cx, JSObject *obj, uintN argc, jsval *argv, jsval *rval)
{
	static	char	fun[] = "StringToPointer";
	void	*r;

	if (argc != 1)
		vError(Dynace, "JavaScript: %s expects %d arguments, given %d.", fun, 1, argc);

	if (!JSVAL_IS_STRING(argv[0]))
		vError(Dynace, "JavaScript:  %s passed an invalid string in arg 1", fun);
	r = (void *) atol(JS_GetStringBytes(JSVAL_TO_STRING(argv[0])));
	*rval = OBJECT_TO_JSVAL(gNewJSPointer(JavaScript, r));
	return JS_TRUE;
}

#if 0
//imeth	gStringRepValue(jsval val)
{
	if (JSVAL_IS_INT(val)) {
		int v = JSVAL_TO_INT(val);
		printf("%d", v);
	} else if (JSVAL_IS_STRING(val)) {
		char *v = JS_GetStringBytes(JSVAL_TO_STRING(val));
		printf("%s", v);
	} else if (JSVAL_IS_DOUBLE(val)) {
		double v = *JSVAL_TO_DOUBLE(val);
		printf("%f", v);
	} else if (JSVAL_IS_BOOLEAN(val)) {
		int v = JSVAL_TO_BOOLEAN(val);
		printf("%s", v ? "true" : "false");
	} else if (JSVAL_IS_NULL(val))
		printf("null");
	else if (JSVAL_IS_VOID(val))
		printf("void");
	else if (JSVAL_IS_OBJECT(val))
		printf("object");
}
#endif

cmeth	gJavaScriptToDynace(void *p)
{
	return gJavaScriptToDynace(cGlobalInstance, p);
}

imeth	gJavaScriptToDynace(void *p)
{
	jsval	val = (jsval) p;

	if (Cache_cx != iCX)
		load_context(iCX);
	
	if (JSVAL_IS_INT(val)) {
		return gNewWithLong(LongInteger, JSVAL_TO_INT(val));
	} else if (JSVAL_IS_STRING(val)) {
		char *v = JS_GetStringBytes(JSVAL_TO_STRING(val));
		return gNewWithStr(String, v);
	} else if (JSVAL_IS_DOUBLE(val)) {
		double v = *JSVAL_TO_DOUBLE(val);
		return gNewWithDouble(DoubleFloat, v);
	} else if (JSVAL_IS_BOOLEAN(val)) {
		int v = JSVAL_TO_BOOLEAN(val);
		return gNewWithLong(ShortInteger, v ? 1 : 0);
	} else if (JSVAL_IS_NULL(val))
		return NULL;
	else if (JSVAL_IS_VOID(val))
		return NULL;
	else if (JSVAL_IS_OBJECT(val)  &&
		 (JS_TRUE == JS_InstanceOf(iCX, (JSObject *) val, DO_classp, NULL) ||
		  JS_TRUE == JS_InstanceOf(iCX, (JSObject *) val, P_classp, NULL)))
		return (object) JS_GetPrivate(iCX, (JSObject *) val);
	else
		return NULL;
}

static	void	class_init()
{
	char	*file;
	object 	jsObj;

	JavaScriptClassSurrogate = CLASS;
	cRT = JS_NewRuntime(0x400000L);
	if (!cRT)
		gError(Dynace, "Can't create JavaScript runtime");
		
	jsObj=gNew(JavaScript);
		
	file = getenv("JAVASCRIPT_INIT");
	if (file  &&  !access(file, 4))
		gExecuteFile(jsObj,file);
	else {
		file = "init.js";
		if (!access(file, 4))
			gExecuteFile(jsObj,file);
	}
}

imeth	char* gLastErrorMsg() {
	object props = (object)JS_GetContextPrivate(iCX);
	if (props && gIsKindOf(props, PropertyList)) {
		object errMsg = gPropertyGet(props, ERRORMSG_PROP);
		if (errMsg && gIsKindOf(errMsg, String))
			return gStringValue(errMsg);
	}
	return 0;
}

cmeth	char* gLastErrorMsg() {
	return gLastErrorMsg(cGlobalInstance);
}

static void clearContextError(JSContext* ctx) {
	object props = (object)JS_GetContextPrivate(ctx);
	if (props && gIsKindOf(props, PropertyList))
		gPropertyRemove(props, ERRORMSG_PROP);
}

static void setContextError(JSContext* ctx, object errmsg) {
	object props = (object)JS_GetContextPrivate(ctx);
	if (props && gIsKindOf(props, PropertyList))
		gPropertyPut(props, ERRORMSG_PROP, 1, errmsg);
}

static JSErrorReporter setErrorReporter(JSContext* ctx, JSErrorReporter rptr, int resetErrors) {
	if (resetErrors)
		clearContextError(ctx);
	return JS_SetErrorReporter(ctx, rptr);
}

void JS_DLL_CALLBACK defaultErrorReporter(JSContext* ctx, const char* msg, JSErrorReport* rpt) {
	if (!JSREPORT_IS_WARNING(rpt->flags)) {
		object buf = vSprintf(String, 	"JavaScript Error: %s", msg);
		
		if (rpt->filename)
			vBuild(buf, NULL, " in ", (strcmp(rpt->filename, "in memory")) ? rpt->filename : "script", END);
		
		if (rpt->lineno)
			vBuild(buf, NULL, Sprintf(NULL, " at line %u", (rpt->lineno)), END);
			
		if (rpt->tokenptr) {
			if (rpt->linebuf)
				vBuild(buf, NULL, Sprintf(NULL, " (position %d)", (rpt->tokenptr - rpt->linebuf) + 1), END);
			vBuild(buf, NULL, ": \"",  rpt->tokenptr, "\"", END);
		}
		
		setContextError(ctx, buf);
		//gMessage(Application, gStringValue(buf));
	}
}

/*Instead of adding the property to the class prototype, simply add it as 
  a property of the class object: i.e. Dynace.Array instead of Dynace.prototype.Array.
*/
imeth gSetJSObjectProp(char *cls, char *prop, obj)
{
	jsval	class_obj, proto;
	JSObject *inst;
	JSClass *cp;
	
	JS_GetProperty(iCX, iGlobalObj, cls, &class_obj);
	inst = gNewJSDynaceObject(self,obj);
	JS_SetProperty(iCX, (JSObject *)class_obj, prop, (jsval *)&inst);
	return self;
	
}

cmeth gSetJSObjectProp(char *cls, char *prop, obj)
{
	return gSetJSObjectProp(cGlobalInstance, cls, prop, obj);
}



