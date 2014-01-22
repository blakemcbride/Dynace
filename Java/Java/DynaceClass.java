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

package Dynace;

import java.util.Hashtable;
import java.lang.reflect.*;

public class DynaceClass extends DynaceBase {

    private static Hashtable m_classmap = new Hashtable();
    private static Hashtable m_classaddrmap = new Hashtable();
    private static Hashtable g_GenericFunctionSigs;

    public DynaceClass(){}

    public DynaceClass(DynaceClass inObj) {
        super((DynaceBase) inObj);
    }

    protected DynaceClass(String szName, int nAddress) {
        super(szName, nAddress);
    }
	
    public static void addClass(String szName, int nAddress) {
        if (!m_classmap.containsKey(szName)) {
            DynaceBase obj = DynaceBase.g_ObjFactory.addDynaceObject(szName, nAddress);
            if (g_ClassOf_object == null)
                g_ClassOf_object = obj.getClass();
            m_classmap.put(szName, obj);
            m_classaddrmap.put(new Integer(obj.getAddress()), szName);
        }
    }

    public static DynaceClass getDynaceClass(String szName) {
        if (!m_classmap.containsKey(szName))
            return null;
        return (DynaceClass)m_classmap.get(szName);
    }

    public static String getClassNameFromAddr(int nAddr) {
        Integer iobj = new Integer(nAddr);
        if (!m_classaddrmap.containsKey(iobj))
            return null;
        return m_classaddrmap.get(iobj).toString();
    }

    private static String FixClassName(String szName) {
        String szParmType = new String("L");
        szParmType = szParmType.concat(szName);
        szParmType = szParmType.concat(";");
        return szParmType.replace('.', '/');
    }

    private static String GetParmType(String szType) {
        String szParmType = new String();
        if (szType.equals("int"))
            szParmType = "I";
        else if (szType.equals("short"))
            szParmType = "S";
        else if (szType.equals("double"))
            szParmType = "D";
        else if (szType.equals("float"))
            szParmType = "F";
        else if (szType.equals("void"))
            szParmType = "V";
        else if (szType.equals("char"))
            szParmType = "C";
        else if (szType.equals("byte"))
            szParmType = "B";
        else
            szParmType = FixClassName(szType);
        
        return szParmType;
    }

    private static String GetMethodSig(Method meth) {
        String szMethSig = new String();
        Class parms[] = meth.getParameterTypes();
        szMethSig = "(";
        for (int ncls = 0; ncls < parms.length; ncls++)
            szMethSig = szMethSig.concat(GetParmType(parms[ncls].getName()));
        
        szMethSig = szMethSig.concat(")");
        szMethSig = szMethSig.concat(GetParmType(meth.getReturnType().getName()));
        return szMethSig;
    }

    private static synchronized void LoadGenericFunctionSigs() {
        if (g_GenericFunctionSigs != null)
            return;
        
        Method gens[];
        g_GenericFunctionSigs = new Hashtable();
        String szPkgName = new String("");
        Package pkg = g_ClassOf_object.getPackage();
        if (pkg != null)
            szPkgName = pkg.getName() + ".";
            
        try {
            gens = ClassLoader.getSystemClassLoader().loadClass(szPkgName + "DynaceObject").getDeclaredMethods();
        } catch (Exception ex) {
            return;
        }
        
        for (int ndx = 0; ndx < gens.length; ndx++) {
            int nMods = gens[ndx].getModifiers();
            if (Modifier.isStatic(nMods))
                continue;
            
            String szName = gens[ndx].getName();
            if (szName.charAt(0) != 'g')
                continue;
            
            String szMethSig = GetMethodSig(gens[ndx]);
            g_GenericFunctionSigs.put(szName, szMethSig);
        }
    }
    
    protected static DynaceClass setupDynaceTwinClass(String szJavaClassName, String szDynaceDeriveClass) {
        String szSuper[] = new String[1];
        szSuper[0] = szDynaceDeriveClass;
        return setupDynaceTwinClass(szJavaClassName, szSuper);
    }

    protected static synchronized DynaceClass setupDynaceTwinClass(String szJavaClassName, String szDynaceDeriveClass[]) {
        DynaceClass dynClass = null;
        try {
            Class cls = ClassLoader.getSystemClassLoader().loadClass(szJavaClassName);
	    
            if (cls == null)
                return null;
            short sArgCnt = 4;
            Object vParms[] = new Object[sArgCnt*2];
            vParms[0] = cls;
            vParms[1] = new String(szJavaClassName);
            vParms[2] = szDynaceDeriveClass;
            vParms[3] = new Integer(szDynaceDeriveClass.length);
            InsertTypes(vParms, sArgCnt);
            callToDynace(getDynaceClass("JavaClass").getAddress(), 
                DynaceGeneric.getAddress("gAddJavaTwinClass"), sArgCnt, vParms);
            
            dynClass = getDynaceClass(szJavaClassName);
            if (dynClass == null)
                return null;
            
            LoadGenericFunctionSigs();
            Method meth[] = cls.getDeclaredMethods();
            for (int ndx = 0; ndx < meth.length; ndx++) {
                String szName = meth[ndx].getName();
                if (!szName.startsWith("ig"))
                    continue;
                if (g_GenericFunctionSigs.containsKey(szName.substring(1))) {
                    //name matches, check parms
                    String szSig = GetMethodSig(meth[ndx]);
                    int nStatic = 0;
                    if (Modifier.isStatic(meth[ndx].getModifiers()))
                        nStatic = 1;
                    if (!szSig.equals((String)g_GenericFunctionSigs.get(szName.substring(1))))
                        continue;
                    if (!Modifier.isPublic(meth[ndx].getModifiers()))
                        continue;
                    sArgCnt = 5;
                    vParms = new Object[sArgCnt*2];
                    vParms[0] = new Integer(dynClass.getAddress());
                    vParms[1] = new String(szName.substring(1));
                    vParms[2] = new Integer(DynaceGeneric.getObjAddress(szName.substring(1)));
                    vParms[3] = new String(szSig);
                    vParms[4] = new Integer(nStatic);
                    InsertTypes(vParms, sArgCnt);
                    callToDynace(getDynaceClass("JavaClass").getAddress(),
                        DynaceGeneric.getAddress("gAddJavaTwinGeneric"), sArgCnt, vParms);
                }
            }
        } catch (Exception excp) {
            Object vParms[] = new Object[2];
            vParms[0] = new String("exception in setupDynaceTwinClass-"	+ excp.getMessage());
            InsertTypes(vParms, (short)1);
            callToDynace(getDynaceClass("Application").getAddress(), 
                         DynaceGeneric.getAddress("gMessage"), (short)1, vParms);
	}
        return dynClass;
    }
}

