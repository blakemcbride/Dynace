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


defclass WebService {
	iHttp;
	iNamespaceURL;
	iMethod;
	iArgs;
};


private imeth	init(char *url)
{
	iHttp = gNewHttpRequest(HttpRequest, url, "POST");
	if (!iHttp)
		return gDispose(self);
	gAddHeader(iHttp, "Content-Type", "text/xml; charset=utf-8");
	gAddHeader(iHttp, "SOAPAction", "NI");
	return self;
}

cmeth	gNewServiceRequest(char *url)
{
	return init(gNew(super), url);
}

imeth	gDispose, gDeepDispose ()
{
	if (iHttp)
		gDispose(iHttp);
	if (iNamespaceURL)
		gDispose(iNamespaceURL);
	if (iMethod)
		gDispose(iMethod);
	if (iArgs)
		gDeepDispose(iArgs);
	return gDispose(super);
}

imeth	gSetNamespaceURL(char *url)
{
	if (iNamespaceURL)
		gDispose(iNamespaceURL);
	iNamespaceURL = gNewWithStr(String, url);
	return self;
}

imeth	gSetMethod(char *meth)
{
	if (iMethod)
		gDispose(iMethod);
	iMethod = gNewWithStr(String, meth);
	return self;
}

imeth	gAddArgument(char *param, char *val)
{
	if (!iArgs)
		iArgs = gNew(LinkObject);
	gAddLast(iArgs, gNewWithObjObj(ObjectAssociation,
				       gNewWithStr(String, param),
				       gNewWithStr(String, val)));
	return self;
}

private	imeth	build_content()
{
	object	content, seq, oa;
	char	buf[256], *p;

	content = vBuild(String,
			 "<?xml version=\"1.0\"?>\n",
			 "<env:Envelope xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\"\n",
			 "\t\txmlns:soaprpc=\"http://www.w3.org/2003/05/soap-rpc\"\n",
			 "\t\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n",
			 "\t\txmlns:xs=\"http://www.w3.org/2001/XMLSchema\"\n",
			 NULL);
	if (iNamespaceURL) {
		sprintf(buf, "\t\txmlns:tns=\"%s\"\n", gStringValue(iNamespaceURL));
		gAppend(content, (object) buf);
	}
	gAppend(content, (object) "\t>\n");
	gAppend(content, (object) "\t<env:Body>\n");
	if (iMethod) {
		sprintf(buf, "\t\t<tns:%s>\n", gStringValue(iMethod));
		gAppend(content, (object) buf);
		if (iArgs)
			for (seq=gSequence(iArgs) ; oa = gNext(seq) ; ) {
				char	*param;

				param = gStringValue(gKey(oa));
				sprintf(buf, "\t\t\t<%s>%s</%s>\n", param, gStringValue(gValue(oa)), param);
				gAppend(content, (object) buf);
			}
		sprintf(buf, "\t\t</tns:%s>\n", gStringValue(iMethod));
		gAppend(content, (object) buf);
	}
	if (iArgs)
		iArgs = gDeepDispose(iArgs);
	gAppend(content, (object) "\t</env:Body>\n");
	gAppend(content, (object) "</env:Envelope>\n");

	return content;
}

imeth	DWORD	gSendRequest(DWORD n)
{
	object	content;
	DWORD	ret;

	content = build_content(self);

	gSetContent(iHttp, gStringValue(content));

	ret = gSendRequest(iHttp, n);

	gDispose(content);

	return ret;
}

imeth	char	*gGetErrorMessage()
{
	return gGetErrorMessage(iHttp);
}

imeth	char	*gGetResponse()
{
	return gGetResponse(iHttp);
}

imeth	gGetXMLResponse()
{
	char	*res = gGetResponse(iHttp);
	return gParseString(XMLNode, res, NULL, NULL, NULL);
}

imeth	char	*gGetResponseHeader(char *headerName)
{
	return gGetResponseHeader(iHttp, headerName);
}




