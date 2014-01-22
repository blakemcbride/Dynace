

defclass MultipartMime {
	iBodyParts;
	iStr;
	char	iBoundaryString[256];
	char	iSubType[32];
	char	iContentType[64];
};

private	imeth	initParseInstance(object self, char *content, char *boundary);
private	imeth	initNewInstance(object self, char *subType, char *boundaryString);
private	cmeth	void	resetString(object *pobj, char *initValue);

cmeth	gNewMultipartMime(char *subType, char *boundaryString)
{
	if (!boundaryString || !subType)
		return NULL;
	return initNewInstance(gNew(super), subType, boundaryString);
}

cmeth	gParseMultipartFromString(char *content, char *boundary)
{
	object newDoc;
	if (!content || !boundary || *content == '\0' || *boundary == '\0')
		return NULL;
	newDoc = gNew(super);
	if (!initParseInstance(newDoc, content, boundary))
		newDoc = gDispose(newDoc);
	return newDoc;
}

private	imeth	initParseInstance(object self, char *content, char *boundary)
{
	char *curPos = NULL, *endPos, boundaryBuf[256], *buf;
	int boundaryLen, lineBreakCharCnt = 0, headerErr = 0;
	object bodyPart;
	
	strncpy(iBoundaryString, boundary, sizeof(iBoundaryString) - 1);
	iBodyParts = gNew(LinkObject);
	*boundaryBuf = boundaryBuf[1] = '-';
	boundaryBuf[2] = '\0';
	strncat(boundaryBuf, boundary, sizeof(boundaryBuf) - 4);
	boundaryLen = strlen(boundaryBuf);
	while (!headerErr) {
		object headerNames, headerMap, bodyPart, seq, val;
		int contentLen;
		char *bufPos;
		
		if (!curPos) {
			curPos = strstr(content, boundaryBuf);
			if (!curPos)
				return NULL;
			curPos += boundaryLen;
		}
		endPos = strstr(curPos, boundaryBuf);
		
		if (!endPos)
			return NULL;

		while (*curPos == '\r' || *curPos == '\n' || *curPos == ' ')
			curPos++;

		contentLen = endPos - curPos;
		buf = malloc(contentLen + 1);
		memcpy(buf, curPos, contentLen);
		buf[contentLen] = '\0';

		//process headers
		bufPos = buf;
		headerNames = gNew(LinkObject);
		headerMap = gNew(StringDictionary);
		while (1) {
			char *headerEndPos, *headerVal, headerEndPosChar;

 			if (*bufPos == '\r' || *bufPos == '\n') {
				if (*bufPos == '\r' && bufPos[1] == '\n')
					bufPos += 2;
				else
					bufPos++;
				break;
			}
			headerEndPos = strpbrk(bufPos, "\r\n");
			if (!headerEndPos) {
				headerErr = 1;
				break;
			}
			headerEndPosChar = *headerEndPos;
			
			*headerEndPos = '\0';
			headerVal = strchr(bufPos, ':');
			if (headerVal) {
				*headerVal++ = '\0';
				while (*headerVal == ' ')
					headerVal++;
				if (*headerVal != '\0') {
					gAddLast(headerNames, gNewWithStr(String, bufPos));
					gAddStr(headerMap, bufPos, gNewWithStr(String, headerVal));
				}
			}
			bufPos = headerEndPos + 1;
			if (headerEndPosChar == '\r' && *bufPos == '\n')
				bufPos++;
		}
		
		if (!headerErr) {
			//process body content
			bodyPart = gNewMimeBodyPart(MimeBodyPart, bufPos);
			for (seq=gSequence(headerNames) ; val = gNext(seq) ; )
				gAddHeader(bodyPart, gStringValue(val), gStringValue(gFindValueStr(headerMap, gStringValue(val))));
			
			gAddLast(iBodyParts, bodyPart);
			curPos = endPos + boundaryLen;
		}
		
		free(buf);
		gDeepDispose(headerNames);
		gDeepDispose(headerMap);
		
		if (!headerErr && *curPos == '-' && curPos[1] == '-')
			break;
	}
	
	return self;
}

private	imeth	initNewInstance(object self, char *subType, char *boundaryString)
{
	iBodyParts = gNew(LinkObject);
	strncpy(iBoundaryString, boundaryString, sizeof(iBoundaryString) - 1);
	strncpy(iSubType, subType, sizeof(iSubType)-1);
	strcpy(iContentType, "multipart/");
	strcat(iContentType, subType);
	return self;
}

imeth	gDispose, gDeepDispose ()
{
	if (iStr)
		gDispose(iStr);
	if (iBodyParts)
		gDeepDispose(iBodyParts);
	return gDispose(super);
}

imeth	gAddBodyPart(object bodyPart)
{
	gAddLast(iBodyParts, bodyPart);
	return self;
}

imeth	char	*gGetContentType()
{
	return iContentType;
}

imeth	char	*gGetBoundaryString()
{
	return iBoundaryString;
}

imeth	gGetBodyParts()
{
	return iBodyParts;
}

imeth	char	*gStringValue()
{
	object bodyPart, seq;
	resetString(MultipartMime, &iStr, "");
	for (seq=gSequence(iBodyParts) ; bodyPart = gNext(seq) ; ) {
		gAppend(iStr, (object)"--");
		gAppend(iStr, (object)iBoundaryString);
		gAppend(iStr, (object)"\r\n");
		gAppend(iStr, (object)gStringValue(bodyPart));
		gAppend(iStr, (object)"\r\n\r\n");
	}
	gAppend(iStr, (object)"--");
	gAppend(iStr, (object)iBoundaryString);
	gAppend(iStr, (object)"--");
	return gStringValue(iStr);
}

private	cmeth	void	resetString(object *pobj, char *initValue) 
{
	if (IsObj(*pobj))
		gDispose(*pobj);
	*pobj = gNewWithStr(String, initValue ? initValue : "");
}



