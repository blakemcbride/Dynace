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

public class DynaceGeneric {

    private static Hashtable m_genericmap = new Hashtable();
    private int m_nAddress, m_nObjAddress;
    String m_szName;

    private DynaceGeneric(String szName, int nAddress, int nObjAddress) {
        m_szName = szName;
        m_nAddress = nAddress;
        m_nObjAddress = nObjAddress;
    }

    public static int getAddress(String szName) {
        if (!m_genericmap.containsKey(szName))
            return 0;
        DynaceGeneric djc = (DynaceGeneric)m_genericmap.get(szName);
        return djc.m_nAddress;
    }

    public static int getObjAddress(String szName) {
        if (!m_genericmap.containsKey(szName))
            return 0;
        DynaceGeneric djc = (DynaceGeneric)m_genericmap.get(szName);
        return djc.m_nObjAddress;
    }

    public static void addGeneric(String szName, int nAddress, int nObjAddress) {
        if (!m_genericmap.containsKey(szName))
            m_genericmap.put(szName, new DynaceGeneric(szName, nAddress, nObjAddress));
        if (szName.equals("DynaceSuperCall"))
            DynaceBase.setSuperCallAddr(nAddress);
    }

    public String toString() {
        return new String(m_szName);
    }
}
