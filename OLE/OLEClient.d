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


#include "oleclnt2.h"

defclass  OLEClient : ComClient {
	IDispatch	*iIP;
};



cmeth	gNewOLEClient(REFCLSID cls)
{
	//  first make connection to server
	object	obj = gNewComClient(super, cls, &IID_IDispatch);
	if (obj) {
		//  connection made -  save pointer to server
		accessIVsOf(obj);
		OLEHEADER(0);
		iIP = (IDispatch *) gPointerValue(super obj);

		//  create an instance of Server
		SetString(0, "Server");
		OLEINVOKE2(1);  //  1=create instance
		OLEEND;
	}
	return obj;
}

imeth	gGCDispose()
{
	if (iIP) {
		//  dispose of instance
		OLEHEADER(0);  //  0 args
		OLEINVOKE2(4);  //  4=DeepDispose
		OLEEND;
	}
	//  drop connection and dispose of local object
	return gDispose(super);
}

imeth	gDispose, gDeepDispose ()
{
	if (iIP) {
		//  dispose of instance
		OLEHEADER(0);  //  0 args
		OLEINVOKE2(4);  //  4=DeepDispose
		OLEEND;
	}
	//  drop connection and dispose of local object
	return gDispose(super);
}











