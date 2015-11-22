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



#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif

#define	USEREG

#include <stdio.h>
#include <string.h>
#include <windows.h>
#ifndef	_WIN32
#ifdef	MAIN
#undef	USEREG
#endif
#include <shellapi.h>
#endif


#ifdef	USEREG

LONG	RegDeleteKeyRecursive(HKEY hkey1, char *sub)
{
	HKEY	hkey2;
	char	name[128];

	if (RegOpenKey(hkey1, sub, &hkey2) == ERROR_SUCCESS) {
		while (RegEnumKey(hkey2, 0, name, (DWORD) sizeof name) == ERROR_SUCCESS)
			RegDeleteKeyRecursive(hkey2, name);
		RegCloseKey(hkey2);
	}
	return RegDeleteKey(hkey1, sub);
	
}

#endif

#define	ISSPACE(c)	(c == ' '  ||  c == '\n'  ||  c == '\r'  ||  c == '\t')

static	char	*rs(char *s)
{
	char	*r = s;

	while (*s)
		s++;
	for (--s ; s >= r  &&  ISSPACE(*s) ; --s);
	s[1] = '\0';
	return r;
}

static	LONG	setValue(char *key, char *value, FILE *fp)
{
	if (fp) {
		fprintf(fp, "HKEY_CLASSES_ROOT\\%s = %s\n", key, value);
		return 0L;
	} else
#ifdef	USEREG
		return RegSetValue(HKEY_CLASSES_ROOT, key, REG_SZ, value, (DWORD)strlen(value));
#else
		return 0L;
#endif
}

static	LONG	delKey(char *key)
{
#ifdef	USEREG
	return RegDeleteKeyRecursive(HKEY_CLASSES_ROOT, key);
#else
	return 0L;
#endif
}

long	SetSystemEnvironmentVariable(int global, char *var, char *val)
{
	HKEY	h;
	DWORD_PTR	r;

	SetEnvironmentVariable(var, val);
	if (global)
		RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment", 0, KEY_ALL_ACCESS, &h);
	else
		RegOpenKeyEx(HKEY_CURRENT_USER, "Environment", 0, KEY_ALL_ACCESS, &h);
	if (val)
		RegSetValueEx(h, var, 0, REG_SZ, val, (DWORD)strlen(val)+1);
	else
		RegDeleteValue(h, var);
	RegCloseKey(h);
	SendMessageTimeout(HWND_BROADCAST,
                        WM_SETTINGCHANGE,
                        0,
                        (LPARAM) "Environment",
                        SMTO_NORMAL	,
                        4000,
                        &r);
	return 0;
}

#define	GET(v)	if (!fgets(v, sizeof v, fp))	goto er1; rs(v)

int	COMRegister(char *file, char *s32, char *s16, char *ofile)
{
	char	full_name[80], company[60], product[60], version[20], id[42], serv32[128], serv16[128], idb[42];
	FILE	*fp = fopen(file, "r"), *ofp=NULL;
	char	key[256], val[132];

	if (!fp)
		return 1;
	if (ofile) {
		ofp = fopen(ofile, "w");
		if (!ofp)
			return 2;
		fprintf(ofp, "REGEDIT\n");
	}
	GET(full_name);
	GET(company);
	GET(product);
	GET(version);
	GET(id);
	GET(serv32);
	GET(serv16);
	fclose(fp);

	if (*id != '{')
		sprintf(idb, "{%s}", id);
	else
		strcpy(idb, id);

	sprintf(key, "%s.%s.%s", company, product, version);
	setValue(key, full_name, ofp);

	sprintf(key, "%s.%s.%s\\CLSID", company, product, version);
	setValue(key, idb, ofp);

	sprintf(key, "%s.%s", company, product);
	setValue(key, full_name, ofp);

	sprintf(key, "%s.%s\\CurVer", company, product);
	sprintf(val, "%s.%s.%s", company, product, version);
	setValue(key, val, ofp);

	sprintf(key, "%s.%s\\CLSID", company, product);
	setValue(key, idb, ofp);

	sprintf(key, "CLSID\\%s", idb);
	setValue(key, full_name, ofp);

	sprintf(key, "CLSID\\%s\\ProgID", idb);
	sprintf(val, "%s.%s.%s", company, product, version);
	setValue(key, val, ofp);

	sprintf(key, "CLSID\\%s\\VersionIndependentProgID", idb);
	sprintf(val, "%s.%s", company, product);
	setValue(key, val, ofp);

	if (*serv32  ||  s32) {
		sprintf(key, "CLSID\\%s\\LocalServer32", idb);
		setValue(key, s32 ? s32 : serv32, ofp);
	}

	if (*serv16  ||  s16) {
		sprintf(key, "CLSID\\%s\\LocalServer", idb);
		setValue(key, s16 ? s16 : serv16, ofp);
	}
	if (ofp)
		fclose(ofp);
	return 0;
er1:
	if (ofp)
		fclose(ofp);
	fclose(fp);
	return 2;
}

int	COMUnRegister(char *file)
{
	char	full_name[80], company[60], product[60], version[20], id[42], idb[42];
	FILE	*fp = fopen(file, "r");
	char	key[256];

	if (!fp)
		return 1;
	GET(full_name);
	GET(company);
	GET(product);
	GET(version);
	GET(id);
	fclose(fp);

	if (*id != '{')
		sprintf(idb, "{%s}", id);
	else
		strcpy(idb, id);

	sprintf(key, "%s.%s.%s", company, product, version);
	delKey(key);

	sprintf(key, "%s.%s", company, product);
	delKey(key);

	sprintf(key, "CLSID\\%s", idb);
	delKey(key);

	return 0;
er1:
	fclose(fp);
	return 2;
}

#ifdef	MAIN

main(int argc, char *argv[])
{
	int	r;
	char	*pgm = argv[0];

#ifdef	USEREG
	if (argc == 4  &&  !stricmp(argv[1], "-suv"))  {
		SetSystemEnvironmentVariable(0, argv[2], argv[3]);
		return 0;
	} else if (argc == 4  &&  !stricmp(argv[1], "-ssv"))  {
		SetSystemEnvironmentVariable(1, argv[2], argv[3]);
		return 0;
	} else if (argc == 3  &&  !stricmp(argv[1], "-esv"))  {
		SetSystemEnvironmentVariable(1, argv[2], NULL);
		return 0;
	} else if (argc == 3  &&  !stricmp(argv[1], "-euv"))  {
		SetSystemEnvironmentVariable(0, argv[2], NULL);
		return 0;
	} else if (!(argc == 3  &&  (!stricmp(argv[1], "-r")  ||  !stricmp(argv[1], "-u")))  &&
	    !(argc == 4  &&  !stricmp(argv[1], "-f")))  {
		fprintf(stderr, "Usage:  %s  -r/-u  input-file\n", pgm);
		fprintf(stderr, "   or:  %s  -f     input-file  output-file\n", pgm);
		fprintf(stderr, "   or:  %s  -suv   variable    value\n", pgm);
		fprintf(stderr, "   or:  %s  -ssv   variable    value\n", pgm);
		fprintf(stderr, "   or:  %s  -euv   variable\n", pgm);
		fprintf(stderr, "   or:  %s  -esv   variable\n", pgm);
		exit(1);
	}
#else
	if (argc != 4  ||  stricmp(argv[1], "-f")) {
		fprintf(stderr, "Usage:  regutil  -f  input-file  output-file\n");
		exit(1);
	}
#endif
	if (!stricmp(argv[1], "-r"))
		r = COMRegister(argv[2], NULL, NULL, NULL);
	else if (!stricmp(argv[1], "-f"))
		r = COMRegister(argv[2], NULL, NULL, argv[3]);
	else
		r = COMUnRegister(argv[2]);

	if (r == 1) {
		fprintf(stderr, "Can't find %s\n", argv[2]);
		exit(1);
	}

	if (r == 2) {
		fprintf(stderr, "File %s has missing lines\n", argv[2]);
		exit(1);
	}
	
	return 0;
}


#endif






