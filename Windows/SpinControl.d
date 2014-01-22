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




defclass SpinControl : Control  {
	object	iSpinCtl;
	int	(*iIncFunc)(object ctl, object dlg, int val);
};

static int scrollFunction(object ctl, unsigned code, unsigned npos);

static int scrollFunction(object ctl, unsigned code, unsigned npos)
{
	object	dlg = gDialog(ctl);
	object	self = gPropertyGet(ctl, "SpinControl");
	accessIVs;

	switch((int) code) {
	case SB_LINEUP:
		if(iIncFunc)
		{
			if (SchemeClassSurrogate  &&  IsObj((object)iIncFunc)  &&  ClassOf(iIncFunc) == String) 
			{
				char	cmd[100], ns[80];
				sprintf(cmd, "(%s (int->object %ld) (int->object %ld) 1)",
					gFunctionName(SchemeClassSurrogate, (object)iIncFunc),
					(long) self, (long) dlg);
				gExecuteInNamespaceNR(SchemeClassSurrogate,
							  gNamespaceName(SchemeClassSurrogate, (object)iIncFunc, ns), 
							  cmd);
			} else if (JavaScriptClassSurrogate  &&  IsObj((object)iIncFunc)  &&  ClassOf(iIncFunc) == JavaScriptString) {
				char	cmd[128];
				sprintf(cmd, "%s(StringToObject(\"%ld\"), StringToObject(\"%ld\"), 1)", gStringValue((object)iIncFunc), (long) self, (long) dlg);
				gExecuteStringNR(JavaScriptClassSurrogate, cmd);
			} else
				iIncFunc(self, dlg, 1);
		}
		break;
	case SB_LINEDOWN:
		if(iIncFunc)
			if (SchemeClassSurrogate  &&  IsObj((object)iIncFunc)  &&  ClassOf(iIncFunc) == String) 
			{
				char	cmd[100], ns[80];
				sprintf(cmd, "(%s (int->object %ld) (int->object %ld) -1)",
					gFunctionName(SchemeClassSurrogate, (object)iIncFunc),
					(long) self, (long) dlg);
				gExecuteInNamespaceNR(SchemeClassSurrogate,
							  gNamespaceName(SchemeClassSurrogate, (object)iIncFunc, ns), 
							  cmd);
			} else if (JavaScriptClassSurrogate  &&  IsObj((object)iIncFunc)  &&  ClassOf(iIncFunc) == JavaScriptString) {
				char	cmd[128];
				sprintf(cmd, "%s(StringToObject(\"%ld\"), StringToObject(\"%ld\"), -1)", gStringValue((object)iIncFunc), (long) self, (long) dlg);
				gExecuteStringNR(JavaScriptClassSurrogate, cmd);
			} else
				iIncFunc(self, dlg, -1);
		break;
	}

	return 0;
}

imeth	gSetSpinFunction(unsigned spinID, int (*ifunc)(object ctl, object dlg, int val))
{
	iIncFunc = ifunc;
	iSpinCtl = gAddControl(gDialog(self), ScrollBar, spinID);
	gPropertyPut(iSpinCtl, "SpinControl", 0, self);
	gSetFunction(iSpinCtl, scrollFunction);
	gDisable(self);
	return self;
}









