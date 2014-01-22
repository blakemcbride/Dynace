

#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _POSIX_
#endif
#endif




defclass MimeBodyPart {
	iStr;
	iHeaders;
	iHeadersAsString;
	iContent;
};

private	imeth	initInstance(object self, char *content);
private	imeth	char	*headersToString(object self);
private	cmeth	void	resetString(object *pobj, char *initValue);

cmeth	gNewMimeBodyPart(char *content)
{
	return initInstance(gNew(super), content);
}

private	imeth	initInstance(object self, char *content)
{
	iHeaders = gNew(LinkObject);
	iContent = gNewWithStr(String, content);
	return self;
}

imeth	gDispose, gDeepDispose ()
{
	if (iStr)
		gDispose(iStr);
	if (iContent)
		gDispose(iContent);
	if (iHeadersAsString)
		gDispose(iHeadersAsString);
	if (iHeaders)
		gDeepDispose(iHeaders);
	return gDispose(super);
}

imeth	gAddHeader(char *name, char *value)
{
	gAddLast(iHeaders, vBuild(String, name, ": ", value, END));
	return self;
}

imeth	gGetHeaders()
{
	return iHeaders;
}

imeth	char	*gGetHeader(char *name)
{
	object header, seq;
	char tmpBuf[256];
	
	for (seq=gSequence(iHeaders) ; header = gNext(seq) ; ) {
		char *valpos, *startPos;
		
		startPos = gStringValue(header);
		strncpy(tmpBuf, startPos, sizeof(tmpBuf));
		valpos = strchr(tmpBuf, ':');
		if (!valpos)
			continue;
		*valpos++ = '\0';
		if (stricmp(tmpBuf, name))
			continue;
		while (*valpos == ' ')
			valpos++;
		gDispose(seq);		
		return startPos + (valpos - tmpBuf);
	}

	return NULL;
}

imeth	char	*gGetBodyContent()
{
	return gStringValue(iContent);
}

private	imeth	char	*headersToString(object self)
{
	object header, seq;
	resetString(MimeBodyPart, &iHeadersAsString, NULL);
	for (seq=gSequence(iHeaders) ; header = gNext(seq) ; ) {
		gAppend(iHeadersAsString, header);
		gAppend(iHeadersAsString, (object)"\r\n");
	}
	
	return gStringValue(iHeadersAsString);
}

imeth	char	*gStringValue()
{
	resetString(MimeBodyPart, &iStr, headersToString(self));
	gAppend(iStr, (object)"\r\n");
	gAppend(iStr, iContent);
	return gStringValue(iStr);
}

private	cmeth	void	resetString(object *pobj, char *initValue) 
{
	if (IsObj(*pobj))
		gDispose(*pobj);
	*pobj = gNewWithStr(String, initValue ? initValue : "");
}


