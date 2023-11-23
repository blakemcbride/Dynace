
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <curl/curl.h>

#define INTERNET_OPTION_CLIENT_CERT_CONTEXT      84

defclass HttpRequest {
	iUrl;
	char		*iResponseStr;
	iHeaders;
	iContent;
	iVerb;
	iUser;
	iPass;
	iOutputFileName;
	iErrorMessage;
	iResponseHeaders;
	int	iSecure;
	int	iTimeoutSeconds;
	void	*iCertificate;
	iInternetOpenOptions;

 class:
	 cLogger;	

 init:	class_init;
};


private	imeth	initInstance(object self, char *url, char *verb);
private imeth	int	restCall();

static char *strlwr(char *p);
static int my_trace(CURL *handle, curl_infotype type, char *data, size_t size, void *userp);

struct data {
	char trace_ascii; /* 1 or 0 */
};
 

cmeth	gNewHttpRequest(char *url, char *verb)
{
	if (!url || !verb)
		return NULL;
	return initInstance(gNew(super), url, verb);
}

cmeth	gSetLogger(logger)
{
	cLogger = logger;
	return self;
}

private	imeth	initInstance(object self, char *url, char *verb)
{
	iVerb = gNewWithStr(String, verb);
	iUrl = gNewWithStr(String, url);
	iHeaders = gNew(LinkObject);
	iInternetOpenOptions = gNew(IntegerDictionary);
	iTimeoutSeconds = 60;
	return self;
}

imeth	gDispose, gDeepDispose ()
{
	if (iResponseStr)
		free(iResponseStr);
	if (iErrorMessage)
		gDispose(iErrorMessage);
	if (iUser)
		gDispose(iUser);
	if (iPass)
		gDispose(iPass);
	if (iVerb)
		gDispose(iVerb);
	if (iContent)
		gDispose(iContent);
	if (iUrl)
		gDispose(iUrl);
	if (iHeaders)
		gDeepDispose(iHeaders);
	if (iResponseHeaders)
		gDeepDispose(iResponseHeaders);
	if (iOutputFileName)
		gDispose(iOutputFileName);
	if (iInternetOpenOptions)
		gDeepDispose(iInternetOpenOptions);
	return gDispose(super);
}

imeth	gSetSecure(int secure)
{
	iSecure = secure;
	return self;
}

imeth	gSetOutputFileName(char *name) 
{
	if (iOutputFileName)
		gDispose(iOutputFileName);
	if (!name)
		return self;
	iOutputFileName = gNewWithStr(String, name);
	return self;
}

imeth	gAddHeader(char *name, char *value)
{
	if (!name || !value)
		return NULL;
	gAddLast(iHeaders, vBuild(String, name, ": ", value, END));
	return self;
}

imeth void gInternetSetOption(int option, char *buffer, int buflen) 
{
	object bufferObj = gNew(String);
	if (buflen <= 0)
		buflen = strlen(buffer);
	gWrite(bufferObj, buffer, buflen);
	gAddInt(iInternetOpenOptions, option, bufferObj);
}

imeth void gInternetSetOptionDirect(int option, void* buffer, int buflen)
{
	UNUSED(iv);
	UNUSED(option);
	UNUSED(buffer);
	UNUSED(buflen);
	gTraceCall(StackTracer);
	return;
}

imeth	void	gSetCertificate(void *cert)
{
	iCertificate = cert;
}

imeth	gSetContent(char *content)
{
	if (iContent)
		iContent = gDispose(iContent);
	if (!content)
		return self;
	iContent = gNewWithStr(String, content);
	return self;
}

imeth	gSetContentFromFile(char *filename)
{
	FILE	*fbuf;
	char	buf[256];
	int		readCnt;
	if (iContent)
		iContent = gDispose(iContent);
	if (!filename)
		return NULL;
	fbuf = fopen(filename, "rb");
	if (fbuf == NULL)
		return NULL;
	iContent = gNewWithStr(String, "");
	while (!feof(fbuf)) {
		readCnt = fread(buf, 1, sizeof(buf) - 1, fbuf);
		if (readCnt > 0) {
			buf[readCnt] = '\0';
			gAppend(iContent, (object)buf);
		}
	}
	
	fclose(fbuf);
	return self;
}

imeth	gSetAuth(char *user, char *pass) 
{
	if (!user || !pass)
		return NULL;
	
	if (iUser)
		iUser = gDispose(iUser);
	iUser = gNewWithStr(String, user);
	
	if (iPass)
		iPass = gDispose(iPass);
	iPass = gNewWithStr(String, pass);
	return self;
}

cmeth	int	gPostAndSaveResponseAsFile(char *url, char *contentType, char *content, int flags, char *outputFileName)
{
	object req = gNewHttpRequest(HttpRequest, url, "POST");
	int	res;
	if (content) {
		gSetContent(req, content);
		gAddHeader(req, "Content-Type", contentType ? contentType : "text/plain");
	}
	gSetOutputFileName(req, outputFileName);
	res = gSendRequest(req, flags);
	gDispose(req);
	return res;
}

imeth	char	*gGetErrorMessage()
{
	if (!iErrorMessage)
		return NULL;
	return gStringValue(iErrorMessage);
}

imeth	char	*gGetResponse()
{
	return iResponseStr;
}

imeth	void	gSetResponseTimeoutSeconds(int seconds)
{
	iTimeoutSeconds = seconds;
}

imeth	gGetResponseHeaders()
{
	return iResponseHeaders;
}

imeth	char	*gGetResponseHeader(char *headerName)
{
	object val;
	char tmpbuf[256];
	if (!iResponseHeaders)
		return NULL;
	strncpy(tmpbuf, headerName, sizeof(tmpbuf) - 1);
	val = gFindValueStr(iResponseHeaders, strlwr(tmpbuf));
	return val ? gStringValue(val) : NULL;
}


static char *strlwr(char *p)
{
	char *r = p;
	for (; *p ; ++p)
		*p = tolower(*p);
	return r;
}

/* holder for curl fetch */
struct curl_fetch_st {
	char *payload;
	size_t size;
};

/* callback for curl fetch */
static size_t curl_callback (void *contents, size_t size, size_t nmemb, void *userp)
{
	size_t realsize = size * nmemb;                             /* calculate buffer size */
	struct curl_fetch_st *p = (struct curl_fetch_st *) userp;   /* cast pointer to fetch struct */

	/* expand buffer using a temporary pointer to avoid memory leaks */
	char * temp = realloc(p->payload, p->size + realsize + 1);

	/* check allocation */
	if (temp == NULL) {
		/* this isn't good */
		LOG_ERROR(cLogger, "ERROR: Failed to expand buffer in curl_callback");
		/* free buffer */
		free(p->payload);
		/* return */
		return 1;
	}

	/* assign payload */
	p->payload = temp;

	/* copy contents to buffer */
	memcpy(&(p->payload[p->size]), contents, realsize);

	/* set new buffer size */
	p->size += realsize;

	/* ensure null termination */
	p->payload[p->size] = 0;

	/* return size */
	return realsize;
}

/* fetch and return url body via curl */
static CURLcode fetch_url(CURL *ch, const char *url, struct curl_fetch_st *fetch, long timeoutSeconds)
{
	CURLcode rcode;                   /* curl result code */
	struct data config;

	config.trace_ascii = 1;
	curl_easy_setopt(ch, CURLOPT_DEBUGFUNCTION, my_trace);
	curl_easy_setopt(ch, CURLOPT_DEBUGDATA, &config);
 	curl_easy_setopt(ch, CURLOPT_VERBOSE, 1L);

	curl_easy_setopt(ch, CURLOPT_MAXREDIRS, -1L);
	curl_easy_setopt(ch, CURLOPT_FOLLOWLOCATION, 1L);
	curl_easy_setopt(ch, CURLOPT_POSTREDIR, CURL_REDIR_POST_ALL);
	curl_easy_setopt(ch, CURLOPT_UNRESTRICTED_AUTH, 1L);
 
	/* init payload */
	fetch->payload = (char *) calloc(1, sizeof(fetch->payload));

	/* check payload */
	if (fetch->payload == NULL) {
		/* log error */
		LOG_ERROR(cLogger, "ERROR: Failed to allocate payload in fetch_url");
		/* return error */
		return CURLE_FAILED_INIT;
	}

	/* init size */
	fetch->size = 0;

	/* set url to fetch */
	curl_easy_setopt(ch, CURLOPT_URL, url);

	/* set calback function */
	curl_easy_setopt(ch, CURLOPT_WRITEFUNCTION, curl_callback);

	/* pass fetch struct pointer */
	curl_easy_setopt(ch, CURLOPT_WRITEDATA, (void *) fetch);

	/* set default user agent */
	curl_easy_setopt(ch, CURLOPT_USERAGENT, "libcurl-agent/1.0");

	/* set timeout */
	curl_easy_setopt(ch, CURLOPT_TIMEOUT, timeoutSeconds);

	/* enable location redirects */
	curl_easy_setopt(ch, CURLOPT_FOLLOWLOCATION, 1L);

	/* set maximum allowed redirects */
	curl_easy_setopt(ch, CURLOPT_MAXREDIRS, 1L);

	LOG_DEBUG(cLogger, "About to run curl_easy_perform()");
	/* fetch the url */
	rcode = curl_easy_perform(ch);
	LOG_DEBUG(cLogger, (char *) curl_easy_strerror(rcode));

	/* return */
	return rcode;
}

private imeth int restCall()
{
	CURL *ch;                                               /* curl handle */
	CURLcode rcode;                                         /* curl result code */

	struct curl_fetch_st curl_fetch;                        /* curl fetch struct */
	struct curl_fetch_st *cf = &curl_fetch;                 /* pointer to fetch struct */
	struct curl_slist *headers = NULL;                      /* http headers to send with request */
	char *verb = gStringValue(iVerb);
	object header, seq;

	curl_fetch.payload = NULL;
	curl_fetch.size = 0;

	/* init curl handle */
	if ((ch = curl_easy_init()) == NULL) {
		if (iErrorMessage)
			gDispose(iErrorMessage);
		iErrorMessage = gNewWithStr(String, "ERROR: Failed to create curl handle in fetch_session");
		/* log error */
		LOG_ERROR(cLogger, "Failed to create curl handle in fetch_session");
		/* return error */
		return CURLE_FAILED_INIT;
	}
	
	for (seq=gSequence(iHeaders) ; header = gNext(seq) ; )
		headers = curl_slist_append(headers, gStringValue(header));
	headers = curl_slist_append(headers, "Expect:");
	
	/* set curl options */
	if (!strcmp(verb, "POST"))
		curl_easy_setopt(ch, CURLOPT_POST, 1L);
	else if (!strcmp(verb, "GET"))
		curl_easy_setopt(ch, CURLOPT_HTTPGET, 1L);
	else if (!strcmp(verb, "PUT"))
		curl_easy_setopt(ch, CURLOPT_PUT, 1L);
    
	curl_easy_setopt(ch, CURLOPT_HTTPHEADER, headers);
	if (!strcmp(verb, "POST") && iContent)
		curl_easy_setopt(ch, CURLOPT_POSTFIELDS, gStringValue(iContent));

	/* fetch page and capture return code */
	rcode = fetch_url(ch, gStringValue(iUrl), cf, (long) iTimeoutSeconds);

	if (rcode != CURLE_OK) {
		if (iErrorMessage)
			gDispose(iErrorMessage);
		iErrorMessage = gNewWithStr(String, (char *) curl_easy_strerror(rcode));
		LOG_DEBUG(cLogger, (char *) curl_easy_strerror(rcode));
	} else
		LOG_DEBUG(cLogger, "HttpRequestLinux - succeeded");

	/* cleanup curl handle */
	curl_easy_cleanup(ch);

	/* free headers */
	curl_slist_free_all(headers);

	/* check return code */
	iResponseStr = cf->payload;

	return rcode;
}

static	void	class_init()
{
	char	logbuf[256];
	CURLcode r = curl_global_init(CURL_GLOBAL_ALL);
	if (r != CURLE_OK) {
		sprintf(logbuf, "curl_global_init() failed %s", curl_easy_strerror(r));
		LOG_ERROR(cLogger, logbuf);
	}
	sprintf(logbuf, "curl version %s", curl_version());
	LOG_INFO(cLogger, logbuf);
}

static void dump(const char *text, FILE *stream, unsigned char *ptr, size_t size, char nohex)
{
	size_t i;
	size_t c;
 
	unsigned int width = 0x10;
	int logLevel = gGetLogLevel(cLogger);
	int fd;
	
	if (logLevel < LOG_LEVEL_DEBUG)
		return;
	fd = gOpenLogFile(cLogger);
	if (fd == STDOUT_FILENO)
		stream = stdout;
	else if (fd == STDERR_FILENO)
		stream = stderr;
	else
		stream = fdopen(fd, "a");
 
	if (nohex)
		/* without the hex output, we can fit more on screen */
		width = 0x40;
 
	fprintf(stream, "%s, %10.10lu bytes (0x%8.8lx)\n",
		text, (unsigned long)size, (unsigned long)size);
 
	for (i = 0; i<size; i += width) {
 
		fprintf(stream, "%4.4lx: ", (unsigned long)i);
 
		if (!nohex) {
			/* hex not disabled, show it */
			for (c = 0; c < width; c++)
				if (i + c < size)
					fprintf(stream, "%02x ", ptr[i + c]);
				else
					fputs("   ", stream);
		}
 
		for (c = 0; (c < width) && (i + c < size); c++) {
			/* check for 0D0A; if found, skip past and start a new line of output */
			if (nohex && (i + c + 1 < size) && ptr[i + c] == 0x0D &&
			    ptr[i + c + 1] == 0x0A) {
				i += (c + 2 - width);
				break;
			}
			fprintf(stream, "%c",
				(ptr[i + c] >= 0x20) && (ptr[i + c]<0x80)?ptr[i + c]:'.');
			/* check again for 0D0A, to avoid an extra \n if it's at width */
			if (nohex && (i + c + 2 < size) && ptr[i + c + 1] == 0x0D &&
			    ptr[i + c + 2] == 0x0A) {
				i += (c + 3 - width);
				break;
			}
		}
		fputc('\n', stream); /* newline */
	}
	fflush(stream);
	gUnlockLogFile(cLogger, fd);
	if (fd != STDOUT_FILENO && fd != STDERR_FILENO)
		fclose(stream);
}
 
static int my_trace(CURL *handle, curl_infotype type, char *data, size_t size, void *userp)
{
  struct data *config = (struct data *)userp;
  const char *text;
  (void)handle; /* prevent compiler warning */
 
  switch(type) {
  case CURLINFO_TEXT:
    LOG_DEBUG(cLogger, data);
    /* FALLTHROUGH */
  default: /* in case a new one is introduced to shock us */
    return 0;
 
  case CURLINFO_HEADER_OUT:
    text = "=> Send header";
    break;
  case CURLINFO_DATA_OUT:
    text = "=> Send data";
    break;
  case CURLINFO_SSL_DATA_OUT:
    text = "=> Send SSL data";
    break;
  case CURLINFO_HEADER_IN:
    text = "<= Recv header";
    break;
  case CURLINFO_DATA_IN:
    text = "<= Recv data";
    break;
  case CURLINFO_SSL_DATA_IN:
    text = "<= Recv SSL data";
    break;
  }
 
  dump(text, stderr, (unsigned char *)data, size, config->trace_ascii);
  return 0;
}
