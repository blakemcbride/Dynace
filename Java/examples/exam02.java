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
/*
 * exam02.java
 *
 * Created on March 26, 2002, 10:20 AM
 */


import Dynace.*;

/**
 *
 * invoke with cmd...
 * wdsj ClassPath LibraryPath com.integra.library.Dynace.wdsexam.DeriveExamples.Exam02.exam02 start com.integra.library.Dynace.wdsexam
 * where ClassPath=path to integra jar and LibraryPath=path of JavaDynace.dll
 */
public class exam02 extends Dynace {

    public static void start() {
        new exam02().go();
    }

    public void go() {
        JavaDouble2 jdbl2 = new JavaDouble2(3.1415927);
        JavaDouble jdbl = new JavaDouble(3.1415927);
        DynaceObject ddbl = new DynaceObject(DoubleFloat.gNewWithDouble(3.1415927));
        Application.gMessage("Derive Class Value = " + jdbl2.gDoubleValue() + ". Base Class Value = " + ddbl.gDoubleValue());
        Application.gMessage("sub-sub-class Dbl Method with parm = " + jdbl2.gSetMFScale(3.1415927));
        Application.gMessage("sub-class Dbl Method with parm = " + jdbl.gSetMFScale(3.1415927));
    }
}
