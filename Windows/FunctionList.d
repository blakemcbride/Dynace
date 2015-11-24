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



defclass  FunctionList : LinkObject;

imeth	object	gDispose, gDeepDispose ()
{
	return gDeepDispose(super);
}

imeth	object	gCopy, gDeepCopy ()
{
	return gDeepCopy(super);
}

imeth	gAddFunctionBefore : addFunBefore(int (*fun)())
{
	object	obj;
	
	if (IsObj((object) fun)  &&  
		(ClassOf(fun) == String ||
		 ClassOf(fun) == JavaScriptString ||
		 ClassOf(fun) == JavaCallbackClassSurrogate))
		obj = (object) fun;
	else
		obj = gNewWithPtr(Pointer, (void *) fun);
	
	gAddFirst(self, obj);
	
	return self;
}

imeth	gAddFunctionAfter : addFunAfter(int (*fun)())
{
	object	obj;
	
	if (IsObj((object) fun)  &&  
		(ClassOf(fun) == String ||
		 ClassOf(fun) == JavaScriptString ||
		 ClassOf(fun) == JavaCallbackClassSurrogate))
		obj = (object) fun;
	else
		obj = gNewWithPtr(Pointer, (void *) fun);
	
	gAddLast(self, obj);
	
	return self;
}

imeth	int	gExecuteFunctionsObjObj(object ctl, object dlg)
{
	int	rval = 0;
	object	seq, obj;

	for (seq=gSequence(self) ; (obj = gNext(seq))  &&  !rval ; )
		if (IsObj((object)obj)  &&  ClassOf((object)obj) == JavaCallbackClassSurrogate)
			return gPerformJavaObjObjCallback((object)obj, ctl, dlg);
		else if (SchemeClassSurrogate  &&  IsObj((object)obj)  &&  ClassOf(obj) == String) {
			char	cmd[100], ns[80];
			object	ret;
			sprintf(cmd, "(%s (int->object %lld) (int->object %lld))",
				gFunctionName(SchemeClassSurrogate, (object) obj),
				PTOLL(ctl), PTOLL(dlg));
			ret = gExecuteInNamespace(SchemeClassSurrogate,
						  gNamespaceName(SchemeClassSurrogate, (object) obj, ns), 
						  cmd);
			if (IsObj(ret)) {
				rval = gLongValue(ret);
				gDispose(ret);
			}
		} else if (JavaScriptClassSurrogate  &&  IsObj((object)obj)  &&  ClassOf(obj) == JavaScriptString) {
			object	ret;
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%lld\"), StringToObject(\"%lld\"))", gStringValue((object)obj), PTOLL(ctl), PTOLL(dlg));
			ret = gExecuteString(JavaScriptClassSurrogate, cmd);
			if (IsObj(ret)) {
				if (ClassOf(ret) == LongInteger)
					rval = gLongValue(ret);
				gDispose(ret);
			}
		} else {
			ifun	fun = (ifun) gPointerValue(obj);
				
			rval = fun(ctl, dlg);
		}

	if (obj)
		gDispose(seq);
	
	return rval;
}

imeth	int	gExecuteFunctionsObj(object dlg)
{
	int	rval = 0;
	object	seq, obj;

	for (seq=gSequence(self) ; (obj = gNext(seq))  &&  !rval ; )
		if (IsObj((object)obj)  &&  ClassOf((object)obj) == JavaCallbackClassSurrogate)
			return gPerformJavaObjCallback((object)obj, dlg);
		else if (SchemeClassSurrogate  &&  IsObj((object)obj)  &&  ClassOf(obj) == String) {
			char	cmd[100], ns[80];
			object	ret;
			sprintf(cmd, "(%s (int->object %lld))",
				gFunctionName(SchemeClassSurrogate, (object) obj), PTOLL(dlg));
			ret = gExecuteInNamespace(SchemeClassSurrogate,
						  gNamespaceName(SchemeClassSurrogate, (object) obj, ns), 
						  cmd);
			if (IsObj(ret)) {
				rval = gLongValue(ret);
				gDispose(ret);
			}
		} else if (JavaScriptClassSurrogate  &&  IsObj((object)obj)  &&  ClassOf(obj) == JavaScriptString) {
			object	ret;
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%lld\"))", gStringValue((object)obj), PTOLL(dlg));
			ret = gExecuteString(JavaScriptClassSurrogate, cmd);
			if (IsObj(ret)) {
				if (ClassOf(ret) == LongInteger)
					rval = gLongValue(ret);
				gDispose(ret);
			}
		} else {
			ifun	fun = (ifun) gPointerValue(obj);
				
			rval = fun(dlg);
		}

	if (obj)	
		gDispose(seq);
	
	return rval;
}

// This function won't pass the hwnd to scheme since it doesn't make sense to do so.
imeth	int	gExecuteFunctionsObjHANDLE(object dlg, HANDLE hwnd)
{
	int	rval = 0;
	object	seq, obj;

	for (seq=gSequence(self) ; (obj = gNext(seq))  &&  !rval ; )
		if (SchemeClassSurrogate  &&  IsObj((object) obj)  &&  ClassOf(obj) == String) {
			char	cmd[100], ns[80];
			object	ret;
			sprintf(cmd, "(%s (int->object %lld))",
				gFunctionName(SchemeClassSurrogate, (object) obj), PTOLL(dlg));
			ret = gExecuteInNamespace(SchemeClassSurrogate,
						  gNamespaceName(SchemeClassSurrogate, (object) obj, ns), 
						  cmd);
			if (IsObj(ret)) {
				rval = gLongValue(ret);
				gDispose(ret);
			}
		} else if (JavaScriptClassSurrogate  &&  IsObj((object)obj)  &&  ClassOf(obj) == JavaScriptString) {
			object	ret;
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%lld\"), StringToPointer(\"%lld\"))", gStringValue((object)obj), PTOLL(dlg), PTOLL(hwnd));
			ret = gExecuteString(JavaScriptClassSurrogate, cmd);
			if (IsObj(ret)) {
				if (ClassOf(ret) == LongInteger)
					rval = gLongValue(ret);
				gDispose(ret);
			}
		} else {
			ifun	fun = (ifun) gPointerValue(obj);
			
			rval = fun(dlg, hwnd);
		}

	if (obj)	
		gDispose(seq);
	
	return rval;
}

imeth	int	gExecuteFunctionsObjInt(object dlg, int res)
{
	int	rval = 0;
	object	seq, obj;

	for (seq=gSequence(self) ; (obj = gNext(seq))  &&  !rval ; )
		if (IsObj((object)obj)  &&  ClassOf((object)obj) == JavaCallbackClassSurrogate)
			return gPerformJavaMenuCallback((object)obj, dlg, res);
		else if (SchemeClassSurrogate  &&  IsObj((object)obj)  &&  ClassOf(obj) == String) {
			char	cmd[100], ns[80];
			object	ret;
			sprintf(cmd, "(%s (int->object %lld) %d)",
				gFunctionName(SchemeClassSurrogate, (object) obj), PTOLL(dlg), res);
			ret = gExecuteInNamespace(SchemeClassSurrogate,
						  gNamespaceName(SchemeClassSurrogate, (object) obj, ns), 
						  cmd);
			if (IsObj(ret)) {
				rval = gLongValue(ret);
				gDispose(ret);
			}
		} else if (JavaScriptClassSurrogate  &&  IsObj((object)obj)  &&  ClassOf(obj) == JavaScriptString) {
			object	ret;
			char	cmd[128];
			sprintf(cmd, "%s(StringToObject(\"%lld\"), %lld)", gStringValue((object)obj), PTOLL(dlg), PTOLL(res));
			ret = gExecuteString(JavaScriptClassSurrogate, cmd);
			if (IsObj(ret)) {
				if (ClassOf(ret) == LongInteger)
					rval = gLongValue(ret);
				gDispose(ret);
			}
		} else {
			ifun	fun = (ifun) gPointerValue(obj);
			
			rval = fun(dlg, res);
		}

	if (obj)	
		gDispose(seq);
	
	return rval;
}





