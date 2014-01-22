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


import Dynace.*;

public class JavaDouble extends DynaceObject {
    public static DynaceClass clsObject;
    static {
        clsObject = setupDynaceTwinClass("JavaDouble", "DoubleFloat");
    }
    
    private String iVar;
    public JavaDouble(double dval) {
        this(((DynaceObject)clsObject.getSuperObj()).gNewWithDouble(dval));
    }

    public JavaDouble(DynaceObject inObj) {
        super(inObj);
        iVar = new String("instance string");
    }
    
    public double igDoubleValue() {
        double retVal;
        String scStr = new String();
        if (getClass().getName().equals("com.integra.library.Dynace.wdsexam.DeriveExamples.Exam02.JavaDouble2"))
            scStr = ", sub-class instance str=" + ((JavaDouble2)this).iSubClassVar;
        Dynace.Application.gMessage("instance str=" + iVar + scStr);
        retVal = ((DynaceObject)clsObject.getSuperObj(this)).gDoubleValue();
        return retVal * -1;
    }

    public double igSetMFScale(double parm1) {
        return parm1 * 2;
    }
}
