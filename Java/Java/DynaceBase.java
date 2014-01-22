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

import java.io.PrintWriter;
import java.io.FileWriter;

public class DynaceBase {
	public static native int callToDynace(int obj, int addr, short sArgCount, Object vArgs[]);
	public static native double callToDynaceReturnDouble(int obj, int addr, short sArgCount, Object vArgs[]);
	public static native int superCallToDynace(int obj, int cls, int addr, short sArgCount, Object vArgs[]);
	public static native double superCallToDynaceReturnDouble(int obj, int cls, int addr, short sArgCount, Object vArgs[]);
	public static native String getStringFromCharBuff(int addr);
	public static native void setSuperCallAddr(int addr);
    
	protected static IObjectFactory g_ObjFactory;
	public static Class g_ClassOf_object;
	protected static int genericAddresses[];

	private boolean m_bSuperCall = false;
	private DynaceClass m_SuperCallCls;
	protected int m_nAddress;
	protected boolean m_bDispose = false;
	String m_szClassName;
    
	static {
		try {
			System.loadLibrary("JavaDynace");
		} catch (java.lang.UnsatisfiedLinkError excp) {
			javax.swing.JOptionPane.showMessageDialog(null, "Error Loading JavaDynace.dll:  {java.library.path}=" + System.getProperty("java.library.path")
								  + " - Message=" + excp.getMessage());
			throw excp;
		}
	}

	public DynaceBase(){}
    
	public DynaceBase(String szClassName, int nAddress) {
		m_nAddress = nAddress;
		m_szClassName = szClassName;
	}
 
	protected DynaceBase(DynaceBase inObj) {
		copy(inObj);
		String szName = getClass().getName();
		if (!szName.equals(g_ClassOf_object.getName())) {
			short sArgCnt = 2;
			Object vParms[] = new Object[sArgCnt*2];
			vParms[0] = this;
			vParms[1] = new Integer(getAddress());
			InsertTypes(vParms, sArgCnt);
			callToDynace(DynaceClass.getDynaceClass("Java").getAddress(),
				     DynaceGeneric.getAddress("gAttachJavaObj"), sArgCnt, vParms);
		} else
			m_bDispose = true;
	}

	protected void finalize() {
		if (m_bDispose)
			callToDynace(getAddress(), DynaceGeneric.getAddress("gDispose"), (short)0, null);
	}

	public static void InsertTypes(Object vParms[], short sArgCnt) {
		for (short ndx = sArgCnt; ndx < sArgCnt * 2; ndx++)
			vParms[ndx] = new String(vParms[ndx-sArgCnt].getClass().getName());
	}
    
	public static void setObjectFactory(IObjectFactory objFactory) {
		g_ObjFactory = objFactory;
	}

	protected boolean getSuperCall() {
		return m_bSuperCall;
	}

	public int getAddress() {
		return m_nAddress;
	}
    
	protected DynaceClass getSuperCallCls() {
		return m_SuperCallCls;
	}

	protected DynaceBase getReturnObj(int nAddr) {
		if (nAddr == 0)
			return null;
		if (nAddr != getAddress())
			return DynaceBase.g_ObjFactory.addDynaceObject(toString(), nAddr);
		else
			return (DynaceBase)this;
	}
 
	public String toString() {
		return new String(m_szClassName);
	}
       
	protected void copy(DynaceBase inObj) {
		m_nAddress = inObj.getAddress();
		m_szClassName = inObj.m_szClassName;
	}

	public static DynaceBase attachDynaceBase(String szClassName, int nAddress) {
		return DynaceBase.g_ObjFactory.addDynaceObject(szClassName, nAddress);
	}
    
	public static DynaceBase attachDynaceBase(int nClassAddr, int nAddress) {
		return attachDynaceBase(DynaceClass.getClassNameFromAddr(nClassAddr), nAddress);
	}
    
	private DynaceBase setSuperCall(DynaceClass cls) {
		m_bSuperCall = true;
		m_SuperCallCls = cls;
		return this;
	}

	public DynaceBase getSuperObj() {
		return DynaceBase.g_ObjFactory.addDynaceObject(toString(), getAddress()).setSuperCall((DynaceClass)this);
	}

	public DynaceBase getSuperObj(DynaceBase obj) {
		return DynaceBase.g_ObjFactory.addDynaceObject(obj.toString(), obj.getAddress()).setSuperCall((DynaceClass)this);
	}

	public DynaceBase igDispose() {
		return null;
	}
    
	public DynaceBase gCheckFunction(Object objContext, String funcname) {
		short sArgCnt = 1;
		Object vParms[] = new Object[sArgCnt*2];
		vParms[0] = new Integer(getCallbackObj(objContext, funcname, "(L" + getClass().getName()
						       + ";L" + getClass().getName() + ";)Ljava/lang/String;").getAddress());
		InsertTypes(vParms, sArgCnt);
		return getReturnObj(callToDynace(getAddress(), DynaceGeneric.getAddress("gCheckFunction"), sArgCnt, vParms));
	}

	public int gSetFunction(Object objContext, String funcname) {
		short sArgCnt = 1;
		Object vParms[] = new Object[sArgCnt*2];
		vParms[0] = new Integer(getCallbackObj(objContext, funcname, "(L" + getClass().getName() + ";L" + getClass().getName() + ";)I").getAddress());
		InsertTypes(vParms, sArgCnt);
		return callToDynace(getAddress(), DynaceGeneric.getAddress("gSetFunction"), sArgCnt, vParms);
	}
    
	public int gSetChgFunction(Object objContext, String funcname) {
		short sArgCnt = 1;
		Object vParms[] = new Object[sArgCnt*2];
		vParms[0] = new Integer(getCallbackObj(objContext, funcname, "(L" + getClass().getName() + ";)I").getAddress());
		InsertTypes(vParms, sArgCnt);
		return callToDynace(getAddress(), DynaceGeneric.getAddress("gSetChgFunction"), sArgCnt, vParms);
	}

	public DynaceBase gAssociate(Object objContext, int nID, String funcname) {
		short sArgCnt = 2;
		Object vParms[] = new Object[sArgCnt*2];
		vParms[0] = new Integer(nID);
		vParms[1] = new Integer(getCallbackObj(objContext, funcname, "(L" + getClass().getName() + ";I)I").getAddress());
		InsertTypes(vParms, sArgCnt);
		return getReturnObj(callToDynace(getAddress(), DynaceGeneric.getAddress("gAssociate"), sArgCnt, vParms));
	}

	private DynaceBase getCallbackObj(Object objContext, String funcname, String methodsig) {
		DynaceClass cls = DynaceClass.getDynaceClass("JavaCallback");
		return cls.gNewJavaCallback(objContext, funcname, methodsig);
	}

	public DynaceBase gNewJavaCallback(Object obj, String parm3, String parm4) {
		short sArgCnt = 4;
		Object vParms[] = new Object[sArgCnt*2];
		vParms[0] = obj;
		vParms[1] = new String(obj.getClass().getName());
		vParms[2] = new String(parm3);
		vParms[3] = new String(parm4);
		InsertTypes(vParms, sArgCnt);
		return getReturnObj(callToDynace(getAddress(), DynaceGeneric.getAddress("gNewJavaCallback"), sArgCnt, vParms));
	}


	public static void logError(Throwable t, String fileName, String msg)	{
		FileWriter writer = null;
		PrintWriter printer = null;

		try {
			writer = new FileWriter(fileName, true);
			printer = new PrintWriter(writer);

			if (msg != null) {
				printer.println(msg);
				printer.println();
			}

			if (t != null) {
				t.printStackTrace(printer);
				printer.println();
			}

			printer.close();
			writer.close();
		}
		catch (Throwable t2) {
		}
		finally	{
			if (printer != null)
				printer.close();
			try {
				if (writer != null)
					writer.close();
			}
			catch (Throwable t3) {
			}
		}
	}
	
}

