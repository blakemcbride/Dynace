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




defclass  Scheme {
 init:	class_init;
 class:
 	DWORD	cSchemeInitThread;
};


#include <string.h>
#include <stdlib.h>
#include <io.h>
#include "scheme.h"
#include <ctype.h>  //  for the WEB stuff

#define	WWWCODE
#define SCHEME_STDOUT	scheme_get_param(scheme_config, MZCONFIG_OUTPUT_PORT)
#define SCHEME_STDERR	scheme_get_param(scheme_config, MZCONFIG_ERROR_PORT)

extern	int	_WDS_Application;

#ifdef NATIVE_THREADS	
extern BOOL WINAPI DllMain_GC(HINSTANCE inst, ULONG reason, LPVOID reserved);
#endif

extern	int	GC_use_registered_statics;

Scheme_Env	*Scheme_global_env;
Scheme_Type	Scheme_Dynace_Object;
Scheme_Type	Scheme_C_Pointer;

static	Scheme_Object	*Scheme_execute_string_with_error(char *str, char *errbuf, int *errflg);

static	Scheme_Object *out_port;
extern Scheme_Object *scheme_string_output_port_type;

static	int	WebOutputPageMain(char *file, int first);

cmeth	gExecuteInNamespace : exeNS (char *ns, char *str)
{
	object	ret;
	char	*buf;

	if (!ns  ||  !*ns)
		return gExecuteString(self, str);
	buf = (char *)malloc(strlen(str) + strlen(ns) + 64);
	sprintf(buf, "(namespace-eval-with-name '%s %s)", ns, str);
	ret = Scheme_to_Dynace(Scheme_execute_string(buf));
	free(buf);
	return ret;
}	 
	
cmeth	gExecuteInNamespaceWithError : exeNSError (char *ns, char *str, char *errbuf, int *errflg)
{
	char	*buf;
	Scheme_Object	*r;

	if (!ns  ||  !*ns)
		return gExecuteStringWithError(self, str, errbuf, errflg);
	buf = (char *)malloc(strlen(str) + strlen(ns) + 64);
	sprintf(buf, "(namespace-eval-with-name '%s %s)", ns, str);
	r = Scheme_execute_string_with_error(buf, errbuf, errflg);
	free(buf);
	return *errflg ? NULL : Scheme_to_Dynace(r);
}

cmeth	void	gExecuteInNamespaceWithErrorNR : exeNSErrorNR (char *ns, char *str, char *errbuf, int *errflg)
{
	char	*buf;

	if (!ns  ||  !*ns)
		gExecuteStringWithErrorNR(self, str, errbuf, errflg);
	else {
		buf = (char *)malloc(strlen(str) + strlen(ns) + 64);
		sprintf(buf, "(namespace-eval-with-name '%s %s)", ns, str);
		Scheme_execute_string_with_error(buf, errbuf, errflg);
		free(buf);
	}
}

cmeth	void	gExecuteInNamespaceNR : exeNSNR (char *ns, char *str)
{
	char	*buf;

	if (!ns  ||  !*ns)
		gExecuteStringNR(self, str);
	else {
		buf = (char *)malloc(strlen(str) + strlen(ns) + 64);
		sprintf(buf, "(namespace-eval-with-name '%s %s)", ns, str);
		Scheme_execute_string(buf);
		free(buf);
	}
}

cmeth	gExecuteString(char *str)
{
	object	ret;
	char	*buf;

	buf = (char *)malloc(strlen(str) * 2 + 128);
	sprintf(buf, "(if (defined? 'namespace-eval-with-name) (namespace-eval-with-name 'global %s) %s)", str, str);
	ret = Scheme_to_Dynace(Scheme_execute_string(buf));
	free(buf);
	return ret;
}

cmeth	gExecuteStringWithError(char *str, char *errbuf, int *errflg)
{
	char	*buf;
	Scheme_Object	*r;

	buf = (char *)malloc(strlen(str) * 2 + 128);
	sprintf(buf, "(if (defined? 'namespace-eval-with-name) (namespace-eval-with-name 'global %s) %s)", str, str);
	r = Scheme_execute_string_with_error(buf, errbuf, errflg);
	free(buf);
	return *errflg ? NULL : Scheme_to_Dynace(r);
}

cmeth	void	gExecuteStringWithErrorNR(char *str, char *errbuf, int *errflg)
{
	char	*buf;

	buf = (char *)malloc(strlen(str) * 2 + 128);
	sprintf(buf, "(if (defined? 'namespace-eval-with-name) (namespace-eval-with-name 'global %s) %s)", str, str);
	Scheme_execute_string_with_error(buf, errbuf, errflg);
	free(buf);
}

cmeth	void	gExecuteStringNR(char *str)
{
	char	*buf;

	buf = (char *)malloc(strlen(str) * 2 + 128);
	sprintf(buf, "(if (defined? 'namespace-eval-with-name) (namespace-eval-with-name 'global %s) %s)", str, str);
	Scheme_execute_string(buf);
	free(buf);
}

cmeth	gExecuteSchemeFile(char *str)
{
	return Scheme_to_Dynace(Scheme_execute_file(str));
}

static	Scheme_Object *make_out_port(void)
{
	if (!out_port)
		out_port = scheme_make_string_output_port();
	return out_port;
}

static	Scheme_Object *get_error_string_output_port(void)
{
	Scheme_Object		*pobj = SCHEME_STDERR;
	Scheme_Output_Port	*op = (Scheme_Output_Port *) pobj;

	if ((!SCHEME_OUTPORTP(pobj)
	     || (op->sub_type != scheme_string_output_port_type))
	    && !out_port)
		out_port = scheme_make_string_output_port();
	else
		out_port = pobj;
	return out_port;
}

cmeth	gSetStringOutputPorts()
{
	Scheme_Object	*op = get_error_string_output_port();
	
	scheme_set_param(scheme_config, MZCONFIG_ERROR_PORT, op);
	scheme_set_param(scheme_config, MZCONFIG_OUTPUT_PORT, op);
	
	return self;
}

Scheme_Object	*scheme_make_string2(char *str)
{
	return scheme_make_string(str ? str : "");
}

static	int	wds_printf(char *fmt, ...)
{
	char	buf[400];

	va_list	marker;
	va_start(marker, fmt);
	vsprintf(buf, fmt, marker);
	va_end(marker);
//	gError(Application, buf);
	gMessage(Application, buf);
	return 1;
}

Scheme_Object	*scheme_new_dynace_object(object obj)
{
	Scheme_Object	*so;

	so = scheme_alloc_object();
	_SCHEME_TYPE(so) = Scheme_Dynace_Object;
	SCHEME_PTR_VAL(so) = (void *) obj;
	return so;
}

Scheme_Object	*scheme_new_c_pointer(void *obj)
{
	Scheme_Object	*so;

	so = scheme_alloc_object();
	_SCHEME_TYPE(so) = Scheme_C_Pointer;
	SCHEME_PTR_VAL(so) = obj;
	return so;
}

SCHEME_FUNCTION(int_to_object)
{
	static	char	fun[] = "int->object";

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	if (!SCHEME_INTP(argv[0]))
		scheme_wrong_type(fun, "integer", 0, argc, argv);
	return scheme_new_dynace_object((object) SCHEME_INT_VAL(argv[0]));
}

SCHEME_FUNCTION(object_to_int)
{
	static	char	fun[] = "object->int";
	
	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	if (!SCHEME_DYNACEP(argv[0]))
		scheme_wrong_type(fun, "Dynace-object", 0, argc, argv);
	return scheme_make_integer((INT_PTR) SCHEME_PTR_VAL(argv[0]));
}

SCHEME_FUNCTION(int_to_pointer)
{
	static	char	fun[] = "int->pointer";

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	if (!SCHEME_INTP(argv[0]))
		scheme_wrong_type(fun, "integer", 0, argc, argv);
	return scheme_new_c_pointer((void *) SCHEME_INT_VAL(argv[0]));
}

SCHEME_FUNCTION(object_to_pointer)
{
	static	char	fun[] = "object->pointer";

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	if (!SCHEME_DYNACEP(argv[0]))
		scheme_wrong_type(fun, "Dynace-object", 0, argc, argv);
	return scheme_new_c_pointer((void *) SCHEME_PTR_VAL(argv[0]));
}

SCHEME_FUNCTION(string_to_function)
{
	static	char	fun[] = "string->function";

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	return scheme_new_c_pointer((void *)gNewWithStr(String, SCHEME_STR_VAL(argv[0])));
}

SCHEME_FUNCTION(pointers_equal)
{
	static	char	fun[] = "pointers-equal?";

	if (argc != 2)
		scheme_wrong_count(fun, 2, 2, argc, argv);
	if (!SCHEME_DYNACEP(argv[0])  &&  !SCHEME_CPOINTERP(argv[0]))
		scheme_wrong_type(fun, "Dynace-object or c-pointer", 0, argc, argv);
	if (!SCHEME_DYNACEP(argv[1])  &&  !SCHEME_CPOINTERP(argv[1]))
		scheme_wrong_type(fun, "Dynace-object or c-pointer", 1, argc, argv);
	return SCHEME_PTR_VAL(argv[0]) == SCHEME_PTR_VAL(argv[1]) ? scheme_true : scheme_false;
}

SCHEME_FUNCTION(dynace_object)
{
	static	char	fun[] = "dynace-object?";

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	return SCHEME_DYNACEP(argv[0]) ? scheme_true : scheme_false;
}

SCHEME_FUNCTION(c_pointer)
{
	static	char	fun[] = "c-pointer?";

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	return SCHEME_CPOINTERP(argv[0]) ? scheme_true : scheme_false;
}

SCHEME_FUNCTION(vSetTabStops_s)
{
	static	char	fun[] = "vSetTabStops";
	int	i, num;
	object	ary, r;

	SCHEME_ARG_VCOUNT(2);
	SCHEME_CHK_ARG_DYNACE(0);
	for (i=1 ; i < argc ; i++)
		SCHEME_CHK_ARG_INT(i);
	num = SCHEME_INT_VAL(argv[argc-1]) ? argc-1 : argc-2;
	ary = vNew(IntegerArray, 1, num);
	for (i=0 ; i < num ; i++)
		vChangeLongValue(ary, (long) SCHEME_INT_VAL(argv[i+1]), i);
	r = gSetTabStopsArray((object) SCHEME_PTR_VAL(argv[0]), ary);
	return scheme_new_dynace_object(r);
}

#ifdef	WWWCODE

static	char	*getWebInputBuffer(void)
{
	static	char	*method = NULL, *buf=NULL;
	int	i;

	if (!method) {
		method = getenv("REQUEST_METHOD");
		if (!method)
			return buf;
		if (!stricmp(method, "POST")) {
			char *clen = getenv("CONTENT_LENGTH");
			int len = clen ? atoi(clen) : 0;
			buf = malloc(len+1);
			for (i=0 ; i < len ; i++)
				buf[i] = getchar();
			buf[i] = '\0';
		} else if (!stricmp(method, "GET"))
			buf = getenv("QUERY_STRING");
	}
	return buf;
}

static	char	*WebValue(char *val, char *name)
{
	char	*buf = getWebInputBuffer();
	int	i, ti, found=0;
	char	temp[1024];

	if (!buf)
		return "";
	for (ti=i=0 ; buf[i] ; i++) {
		char	c;

		c = buf[i];
		if (c == '=') {
			temp[ti] = '\0';
			ti = 0;
			if (!stricmp(temp, name))
				found = 1;
		} else if (c == '&') {
			if (found)
				break;
			ti = 0;
		} else {
			if (c == '+')
				c = ' ';
			else if (c == '%') {
				i++;
				c = 16 * (buf[i] >= 'A' ? ((buf[i] & 0xdf) - 'A') + 10 : buf[i] - '0');
				i++;
				c += buf[i] >= 'A' ? ((buf[i] & 0xdf) - 'A') + 10 : buf[i] - '0';
			}
			if (ti < sizeof(temp)-1)
				temp[ti++] = c;
		}
	}
	temp[found ? ti : 0] = '\0';
	strcpy(val, temp);
	return val;
}

static	object	WebListboxValues(char *name)
{
	char	*buf = getWebInputBuffer();
	int	i, ti, found=0;
	char	temp[1024];
	object	rval = gNew(LinkObject);

	if (!buf)
		return rval;
	for (ti=i=0 ; buf[i] ; i++) {
		char	c;

		c = buf[i];
		if (c == '=') {
			temp[ti] = '\0';
			ti = 0;
			if (!stricmp(temp, name))
				found = 1;
		} else if (c == '&') {
			if (found) {
				temp[ti] = '\0';
				gAddLast(rval, gNewWithStr(String, temp));
				found = 0;
			}
			ti = 0;
		} else {
			if (c == '+')
				c = ' ';
			else if (c == '%') {
				i++;
				c = 16 * (buf[i] >= 'A' ? ((buf[i] & 0xdf) - 'A') + 10 : buf[i] - '0');
				i++;
				c += buf[i] >= 'A' ? ((buf[i] & 0xdf) - 'A') + 10 : buf[i] - '0';
			}
			if (ti < sizeof(temp)-1)
				temp[ti++] = c;
		}
	}
	temp[found ? ti : 0] = '\0';
	if (*temp)
		gAddLast(rval, gNewWithStr(String, temp));
	return rval;
}

static	char	*WebReferer(void)
{
	char	*r = getenv("HTTP_REFERER");
	return r ? r : "";
}

static	char	*WebPage(char *buf)
{
	char	*p = WebReferer();
	char	*r = p;

	for (r=p ; *p ; ++p)
		if ((*p == '/'  ||  *p == '\\')  &&  *(p+1))
			r = p + 1;
	strcpy(buf, r);
	for (p = buf + strlen(buf) - 1 ; p > buf ; --p)
		if (*p == '.') {
			*p = '\0';
			break;
		}
	return buf;
}

SCHEME_FUNCTION(_WebReferer)
{
	static	char	fun[] = "WebReferer";

	if (argc != 0)
		scheme_wrong_count(fun, 0, 0, argc, argv);
	return scheme_make_string2(WebReferer());
}

SCHEME_FUNCTION(_WebPage)
{
	static	char	fun[] = "WebPage";
	char	buf[128];

	if (argc != 0)
		scheme_wrong_count(fun, 0, 0, argc, argv);
	return scheme_make_string2(WebPage(buf));
}

SCHEME_FUNCTION(_WebValue)
{
	static	char	fun[] = "WebValue";
	char	buf[1024];

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	return scheme_make_string2(WebValue(buf, SCHEME_STR_VAL(argv[0])));
}

SCHEME_FUNCTION(_WebListboxValues)
{
	static	char	fun[] = "WebListboxValues";

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	return scheme_new_dynace_object(WebListboxValues(SCHEME_STR_VAL(argv[0])));
}

static	object	ValueDictionary;

static	void	WebSetValue(char *name, char *value)
{
	if (!ValueDictionary)
		ValueDictionary = gNewWithInt(StringDictionary, 101);
	gAddStr(ValueDictionary, name, gNewWithStr(String, value));
}

static	void	WebSetValues(char *name, char *val1, char *val2)
{
	object	lo;

	if (!ValueDictionary)
		ValueDictionary = gNewWithInt(StringDictionary, 101);
	lo = gFindValueStr(ValueDictionary, name);
	if (!lo)
		gAddStr(ValueDictionary, name, lo = gNew(LinkObject));
	gAddLast(lo, gNewWithStrObj(StringAssociation, val1, gNewWithStr(String, val2)));
}

static	void	WebSetListbox(char *name, char *value, char *display, int selected)
{
	object	lo;

	if (!ValueDictionary)
		ValueDictionary = gNewWithInt(StringDictionary, 101);
	lo = gFindValueStr(ValueDictionary, name);
	if (!lo)
		gAddStr(ValueDictionary, name, lo = gNew(LinkObject));
	gAddLast(lo, vMakeList(LinkObject, gNewWithStr(String, value),
			       gNewWithStr(String, display), 
			       gNewWithInt(ShortInteger, selected),
			       NULL));
}

static	void	WebSetRadio(char *name, char *value, int selected)
{
	object	lo;
	char	name2[128];

	sprintf(name2, "%s@%s", name, value);

	if (!ValueDictionary)
		ValueDictionary = gNewWithInt(StringDictionary, 101);
	lo = gFindValueStr(ValueDictionary, name2);
	if (!lo)
		gAddStr(ValueDictionary, name2, lo = gNew(LinkObject));
	gAddLast(lo, gNewWithInt(ShortInteger, selected));
}

SCHEME_FUNCTION(_WebSetValue)
{
	static	char	fun[] = "WebSetValue";

	if (argc != 2  &&  argc != 3)
		scheme_wrong_count(fun, 2, 3, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	if (!SCHEME_STRINGP(argv[1]))
		scheme_wrong_type(fun, "string", 1, argc, argv);
	if (argc == 3  &&  !SCHEME_STRINGP(argv[2]))
		scheme_wrong_type(fun, "string", 2, argc, argv);
	if (argc == 2)
		WebSetValue(SCHEME_STR_VAL(argv[0]), SCHEME_STR_VAL(argv[1]));
	else
		WebSetValues(SCHEME_STR_VAL(argv[0]), SCHEME_STR_VAL(argv[1]), SCHEME_STR_VAL(argv[2]));
	return scheme_true;
}

SCHEME_FUNCTION(_WebSetText)
{
	static	char	fun[] = "WebSetText";

	if (argc != 2)
		scheme_wrong_count(fun, 2, 2, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	if (!SCHEME_STRINGP(argv[1]))
		scheme_wrong_type(fun, "string", 1, argc, argv);
	WebSetValue(SCHEME_STR_VAL(argv[0]), SCHEME_STR_VAL(argv[1]));
	return scheme_true;
}

SCHEME_FUNCTION(_WebSetStatic)
{
	static	char	fun[] = "WebSetStatic";

	if (argc != 2)
		scheme_wrong_count(fun, 2, 2, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	if (!SCHEME_STRINGP(argv[1]))
		scheme_wrong_type(fun, "string", 1, argc, argv);
	WebSetValue(SCHEME_STR_VAL(argv[0]), SCHEME_STR_VAL(argv[1]));
	return scheme_true;
}

SCHEME_FUNCTION(_WebSetCheckbox)
{
	static	char	fun[] = "WebSetCheckbox";

	if (argc != 1)
		scheme_wrong_count(fun, 2, 2, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	WebSetValue(SCHEME_STR_VAL(argv[0]), "");
	return scheme_true;
}

SCHEME_FUNCTION(_WebSetListbox)
{
	static	char	fun[] = "WebSetListbox";

	if (argc != 4)
		scheme_wrong_count(fun, 4, 4, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	if (!SCHEME_STRINGP(argv[1]))
		scheme_wrong_type(fun, "string", 1, argc, argv);
	if (!SCHEME_STRINGP(argv[2]))
		scheme_wrong_type(fun, "string", 2, argc, argv);
	SCHEME_CHK_ARG_INT(3);
	WebSetListbox(SCHEME_STR_VAL(argv[0]), SCHEME_STR_VAL(argv[1]), SCHEME_STR_VAL(argv[2]), SCHEME_INT_VAL(argv[3]));
	return scheme_true;
}

SCHEME_FUNCTION(_WebSetRadio)
{
	static	char	fun[] = "WebSetRadio";

	if (argc != 3)
		scheme_wrong_count(fun, 3, 3, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	if (!SCHEME_STRINGP(argv[1]))
		scheme_wrong_type(fun, "string", 1, argc, argv);
	SCHEME_CHK_ARG_INT(2);
	WebSetRadio(SCHEME_STR_VAL(argv[0]), SCHEME_STR_VAL(argv[1]), SCHEME_INT_VAL(argv[2]));
	return scheme_true;
}

/*
  in is TYPE="ABC" or TYPE("ABC"  ; this function returns a pointer to the A
  out gets a copy of ABC
  beg gets a pointer to the beginning of the entire string found
  NULL is returned if TYPE is not found
*/

#define	SKIP_SPACE	for ( ; in[i] == ' ' ; i++)


static	char	*get_value(char *in, char *out, char *type, char **beg)
{
	int	i, j;
	char	*r = NULL;
	
	for ( ; *in ; in++) {
		for (j=i=0 ; type[j] &&  tolower(type[j]) == tolower(in[i]) ; ) {
			if (!i  &&  beg)
				*beg = in;
			while (type[++j] == ' ');
			while (in[++i] == ' ');
		}
		if (!type[j]  &&  in[i]) {
			SKIP_SPACE;
			if (in[i] != '='  &&  in[i] != '(')
				continue;
			i++;
			SKIP_SPACE;
			if (in[i] == '"') {
				in += i + 1;
				r = in;
				while (*in  &&  *in != '"')
					*out++ = *in++;
				*out = '\0';
				break;
			}
		}
	}
	return r;
}

static	void	handle_text(char *line)
{
	char	name[128], val[256];
	char	c, *p = get_value(line, name, "NAME", NULL);
	object	v;
	if (p) {
		p += strlen(name) + 1;  // point to just after the closing "
		c = *p;
		*p = '\0';
		fputs(line, stdout);
		*p = c;
		line = p;
		p = get_value(line, val, "VALUE", NULL);
		if (p) {
			c = *p;
			*p = '\0';
			fputs(line, stdout);
			*p = c;

			v = gFindValueStr(ValueDictionary, name);
			if (v  &&  ClassOf(v) == String)
				fputs(gStringValue(v), stdout);
			else
				fputs(val, stdout);
			
			for (; *p  &&  *p != '"' ; ++p);
			fputs(p, stdout);
		} else {
			v = gFindValueStr(ValueDictionary, name);
			if (v  &&  ClassOf(v) == String)
				printf(" VALUE=\"%s\"", gStringValue(v));
			fputs(line, stdout);
		}
	} else
		fputs(line, stdout);
}

static	void	handle_request(char *line, char *name, char *p, char *beg)
{
	object	v;

	if (p) {
		*beg = '\0';
		fputs(line, stdout);
		v = gFindValueStr(ValueDictionary, name);
		if (v  &&  ClassOf(v) == String)
			fputs(gStringValue(v), stdout);
		while (*p  &&  *p != '>')
			p++;
		if (*p  &&  *++p)
			fputs(p, stdout);
	} else
		fputs(line, stdout);
}

static	void	handle_checkbox(char *line)
{
	char	name[128];
	char	c, *p = get_value(line, name, "<INPUT TYPE", NULL);
	object	v;

	if (p) {
		for (; *p  &&  *p != '"' ; ++p);
		if (*p) {
			p += 2;
			c = *p;
			*p = '\0';
			fputs(line, stdout);
			*p = c;

			for ( ; *p == ' ' ; p++);
			if (!strnicmp(p, "CHECKED", 7)) {
				fputs(p, stdout);
				return;
			}
			line = p;

			p = get_value(line, name, "NAME", NULL);
			if (p) {
				v = gFindValueStr(ValueDictionary, name);
				if (v)
					printf("CHECKED %s", line);
				else
					fputs(line, stdout);
			} else
				fputs(line, stdout);
		} else
			fputs(line, stdout);
	} else
		fputs(line, stdout);
}

static	void	handle_radiobutton(char *line)
{
	char	name[128], val[128];
	char	c, *p = get_value(line, name, "<INPUT TYPE", NULL);
	object	v;

	if (p) {
		for (; *p  &&  *p != '"' ; ++p);
		if (*p) {
			p += 2;
			c = *p;
			*p = '\0';
			fputs(line, stdout);
			*p = c;

			for ( ; *p == ' ' ; p++);
			if (!strnicmp(p, "CHECKED", 7)) {
				fputs(p, stdout);
				return;
			}
			line = p;

			p = get_value(line, name, "NAME", NULL);
			if (p) {
				p = get_value(p, val, "VALUE", NULL);
				if (p) {
					char	both[128];
					sprintf(both, "%s@%s", name, val);
					v = gFindValueStr(ValueDictionary, both);
					if (!v)
						v = gFindValueStr(ValueDictionary, name);
					if (v  &&  ((ClassOf(v) == ShortInteger  &&  gShortValue(v))  ||  ClassOf(v) != ShortInteger))
						printf("CHECKED %s", line);
					else
						fputs(line, stdout);
				} else
					fputs(line, stdout);
			} else
				fputs(line, stdout);
		} else
			fputs(line, stdout);
	} else
		fputs(line, stdout);
}
				
static	void	handle_listbox(char *line)
{
	char	name[128];
	char	*p = get_value(line, name, "NAME", NULL);

	if (p) {
		object	v;  // a LinkObject of StringAssociations objects
		char	c, *t;

		for (t=p ; *t &&  *t != '<' ; t++);
		c = *t;
		*t = '\0';
		fputs(line, stdout);
		*t = c;
		line = t;

		v = gFindValueStr(ValueDictionary, name);
		if (v  &&  ClassOf(v) == LinkObject) {
			object	seq, val;
			for (seq=gSequence(v) ; val = gNext(seq) ; )
				if (ClassOf(val) == StringAssociation)  //  old code
					printf("<OPTION VALUE=\"%s\">%s\n", gStringKey(val), gStringValue(gValue(val)));
				else if (ClassOf(val) == LinkObject) {  //  new code
					object	value, display, selected;
					vGetValues(val, &value, &display, &selected, NULL);
					if (selected  &&  ClassOf(selected) == ShortInteger)  //  right structure
						printf("<OPTION VALUE=\"%s\"%s>%s\n",
						       gStringValue(value),
						       gShortValue(selected) ? " SELECTED " : "", 
						       gStringValue(display));
				}
		}

		if (*line)
			fputs(line, stdout);
	} else
		fputs(line, stdout);
}

static	void	process(char *line)
{
	char	type[128], *p, *beg;

	if (get_value(line, type, "<INPUT TYPE", NULL)) {
		if (!stricmp(type, "TEXT")  ||  !stricmp(type, "HIDDEN"))
			handle_text(line);
		else if (!stricmp(type, "CHECKBOX"))
			handle_checkbox(line);
		else if (!stricmp(type, "RADIO"))
			handle_radiobutton(line);
		else
			fputs(line, stdout);
	} else if (get_value(line, type, "<SELECT NAME", NULL)) {
		handle_listbox(line);
	} else if (p=get_value(line, type, "<% = REQUEST", &beg)) {
		handle_request(line, type, p, beg);
	} else if (p=get_value(line, type, "<!-- #INCLUDE FILE", NULL)) {
		WebOutputPageMain(type, 0);
	} else
		fputs(line, stdout);
}

static	int	WebOutputPageMain(char *file, int first)
{
	FILE	*fp;
	char	buf[1024];

	fp = fopen(file, "r");
	if (!fp)
		return 0;
	if (!ValueDictionary)
		ValueDictionary = gNewWithInt(StringDictionary, 101);
	if (first)
		fputs("Content-type: text/html\n\n", stdout);
	while (fgets(buf, sizeof buf, fp))
		process(buf);
	fclose(fp);
	return 1;
}

static	int	WebOutputPage(char *file)
{
	return WebOutputPageMain(file, 1);
}

SCHEME_FUNCTION(_WebOutputPage)
{
	static	char	fun[] = "WebOutputPage";

	if (argc != 1)
		scheme_wrong_count(fun, 1, 1, argc, argv);
	if (!SCHEME_STRINGP(argv[0]))
		scheme_wrong_type(fun, "string", 0, argc, argv);
	return WebOutputPage(SCHEME_STR_VAL(argv[0])) ? scheme_true : scheme_false;
}

SCHEME_FUNCTION(IsObj)
{
	static	char	fun[] = "IsObj";
	object	obj;

	SCHEME_ARG_COUNT(1);
	SCHEME_CHK_ARG_DYNACE(0);

	obj = (object) SCHEME_PTR_VAL(argv[0]);

	return IsObj(obj) ? scheme_true : scheme_false;
}

SCHEME_FUNCTION(sGets)
{
	static	char	fun[] = "sGets";
	char	buf[1024];

	SCHEME_ARG_COUNT(1);
	SCHEME_CHK_ARG_DYNACE(0);
	return scheme_make_string(gGets((object) SCHEME_PTR_VAL(argv[0]), buf, sizeof(buf)-1));
}

extern	Scheme_Object *scheme_make_sized_string(char *chars, long len, int copy);

SCHEME_FUNCTION(sRead)
{
	static	char	fun[] = "sRead";
	int	r, sz;
	char	*buf;
	Scheme_Object *ret;
	
	SCHEME_ARG_COUNT(2);
	SCHEME_CHK_ARG_DYNACE(0);
	SCHEME_CHK_ARG_INT(1);
	sz = SCHEME_INT_VAL(argv[1]);
	buf = malloc(sz+1);
	r = gRead((object) SCHEME_PTR_VAL(argv[0]),
		  buf,
		  sz);
	if (r >= 0)
		buf[r] = '\0';
	else
		buf[0] = '\0';
	ret = scheme_make_sized_string(buf, r<0?0:r, 1);
	free(buf);
	return ret;
}
	

#endif

#ifdef NATIVE_THREADS	
void	Scheme_DllMain(HINSTANCE hinst, ULONG reason, LPVOID reserved)
{
	static	int	calledScheme_DllMain = 0;
	if (calledScheme_DllMain)
		return;
	calledScheme_DllMain = 1;
	DllMain_GC(hinst, reason, NULL);
}
#endif

void	Scheme_init(void)
{
	if (Scheme_global_env)
		return;
	scheme_case_sensitive = 1;
	if (_WDS_Application) {
		scheme_console_printf = wds_printf;
		scheme_make_stdout = scheme_make_stderr = make_out_port;
	}

	GC_use_registered_statics = 1;  //  Needed for the Java interface
#ifdef NATIVE_THREADS	
	Scheme_DllMain(NULL, DLL_PROCESS_ATTACH, NULL);
#endif
	Scheme_global_env = scheme_basic_env();
	Scheme_Dynace_Object = scheme_make_type("<Dynace-object>");
	Scheme_C_Pointer = scheme_make_type("<C-Pointer>");
	ADD_FUN(int->object, int_to_object);
	ADD_FUN(object->int, object_to_int);
	ADD_FUN(int->pointer, int_to_pointer);
	ADD_FUN(object->pointer, object_to_pointer);
	ADD_FUN(string->function, string_to_function);
	ADD_FUN(pointers-equal?, pointers_equal);
	ADD_FUN(dynace-object?, dynace_object);
	ADD_FUN(c-pointer?, c_pointer);
	ADD_FUN(vSetTabStops, vSetTabStops_s);
#ifdef	WWWCODE
	ADD_FUN(WebReferer, _WebReferer);
	ADD_FUN(WebPage, _WebPage);
	ADD_FUN(WebValue, _WebValue);
	ADD_FUN(WebListboxValues, _WebListboxValues);
	ADD_FUN(WebOutputPage, _WebOutputPage);
	ADD_FUN(WebSetValue, _WebSetValue);
	ADD_FUN(WebSetText, _WebSetText);
	ADD_FUN(WebSetStatic, _WebSetStatic);
	ADD_FUN(WebSetCheckbox, _WebSetCheckbox);
	ADD_FUN(WebSetListbox, _WebSetListbox);
	ADD_FUN(WebSetRadio, _WebSetRadio);
#endif
	ADD_FUN(IsObj, IsObj);
	ADD_FUN(sGets, sGets);
	ADD_FUN(sRead, sRead);

	Scheme_init_base();
	Scheme_init_app();
}

static	void	handle_error()
{
	if (_WDS_Application)
		gError(Object, scheme_get_string_output(get_error_string_output_port()));
	else
		exit(1);
}

Scheme_Object	*Scheme_execute_string(char *str)
{
	jmp_buf	jb;
	Scheme_Object	*r;

#ifdef NATIVE_THREADS	
	if (cSchemeInitThread != GetCurrentThreadId() && !gCheckThreadInPool(SchemeThread))
		return (Scheme_Object *)vExecuteInPoolThread(SchemeThread, Scheme_execute_string, 1, str);
#endif
	if (!Scheme_global_env)
		Scheme_init();

	memcpy(&jb, &scheme_error_buf, sizeof scheme_error_buf);
	if (scheme_setjmp(scheme_error_buf))
		handle_error();
	r = scheme_eval_string_multi(str, Scheme_global_env);
	memcpy(&scheme_error_buf, &jb, sizeof scheme_error_buf);
	return r;
}

static	Scheme_Object	*Scheme_execute_string_with_error(char *str, char *errbuf, int *errflg)
{
	jmp_buf	jb;
	Scheme_Object	*r;

#ifdef NATIVE_THREADS	
	if (cSchemeInitThread != GetCurrentThreadId() && !gCheckThreadInPool(SchemeThread))
		return (Scheme_Object *)vExecuteInPoolThread(SchemeThread, Scheme_execute_string_with_error, 3, str, errbuf, errflg);
#endif
	if (!Scheme_global_env)
		Scheme_init();

	memcpy(&jb, &scheme_error_buf, sizeof scheme_error_buf);
	if (scheme_setjmp(scheme_error_buf)) {
		*errflg = 1;
		if (errbuf)
			strcpy(errbuf, scheme_get_string_output(get_error_string_output_port()));
		scheme_reset_string_output_port(get_error_string_output_port());
		memcpy(&scheme_error_buf, &jb, sizeof scheme_error_buf);
		return NULL;
	}
	r = scheme_eval_string_multi(str, Scheme_global_env);
	memcpy(&scheme_error_buf, &jb, sizeof scheme_error_buf);
	*errflg = 0;
	return r;
}

Scheme_Object	*Scheme_execute_file(char *file)
{
	char	buf[256];

	sprintf(buf, "(if (defined? 'require-file) (require-file \"%s\") (load/use-compiled \"%s\"))", file, file);
	return Scheme_execute_string(buf);
}

object	Scheme_to_Dynace(Scheme_Object *r)
{
#ifdef NATIVE_THREADS	
	if (cSchemeInitThread != GetCurrentThreadId() && !gCheckThreadInPool(SchemeThread))
		return (object)vExecuteInPoolThread(SchemeThread, Scheme_to_Dynace, 1, r);
#endif
	if (SCHEME_MVP(r)) {				//  Multiple Values
		int	i;
		object	r = gNew(LinkObject);

		for (i=0 ; i < scheme_multiple_count ; i++)
			gAddLast(r, Scheme_to_Dynace(scheme_multiple_array[i]));
		return r;
	} else if (SCHEME_LISTP(r)) {
		int	i;
		object	rval = gNew(LinkObject);
		object	obj;

		while (SCHEME_PAIRP(r)) {
			if (obj = Scheme_to_Dynace(SCHEME_CAR(r)))
				gAddLast(rval, obj);
			r = SCHEME_CDR(r);
		}
		if (!SCHEME_NULLP(r))
			gAddLast(rval, Scheme_to_Dynace(r));
		return rval;
	} else if (SCHEME_CHARP(r))
		return gNewWithChar(Character, SCHEME_CHAR_VAL(r));
	else if (SCHEME_INTP(r))
		return gNewWithLong(LongInteger, SCHEME_INT_VAL(r));
	else if (SCHEME_DBLP(r))
		return gNewWithDouble(DoubleFloat, SCHEME_DBL_VAL(r));
	else if (SCHEME_STRINGP(r))
		return gNewWithStr(String, SCHEME_STR_VAL(r));
	else if (SCHEME_NULLP(r))
		return NULL;
	else if (SCHEME_DYNACEP(r))
		return (object) SCHEME_PTR_VAL(r);
	else if (SCHEME_CPOINTERP(r))
		return (object) SCHEME_PTR_VAL(r);
	return NULL;
}

cmeth	char	*gFunctionName(obj)
{
	char	*p, *buf = gStringValue(obj);

	for (p=buf ; *p ; p++)
		if (p[0] == ':'  &&  p[1] == ':')
			return p + 2;
	return buf;
}

cmeth	char	*gNamespaceName(obj, char *ns)
{
	char	*p, *buf = gStringValue(obj);

	for (p=buf ; *p ; p++)
		if (p[0] == ':'  &&  p[1] == ':') {
			char	*pns = ns;
			while (buf != p)
				*pns++ = *buf++;
			*pns = '\0';
			return ns;
		}
	*ns = '\0';
	return NULL;
}

static	void	class_init()
{
	char	*file;
	
	SchemeClassSurrogate = CLASS;

	cSchemeInitThread = GetCurrentThreadId();
	file = getenv("SCHEME_INIT");
	if (file  &&  !access(file, 4))
		Scheme_execute_file(file);
	else {
		file = "init.scm";
		if (!access(file, 4))
			Scheme_execute_file(file);
	}
#ifdef NATIVE_THREADS	
	gInitSchemeThreadPool(SchemeThread);
#endif
}









