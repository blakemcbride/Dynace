#include "generics.h"
#include "devices.h"

#define MODE_CFILE	1
#define MODE_SFILE	2
#define MODE_HFILE	4

void	force_link()  /*  force the console objects to be loaded instead of the Windows ones  */
{
	Abort2("sd", __FILE__, __LINE__);
}

static	void	usage(void)
{
	printf("\nUsage: SchemeResource file\n"
	       "       where file is the name of the resource (RC) file.\n\n");
}

static	void	addEntryToDict(object dict, char *line)
{
	char	*b, *e;

	for (b = line + 1; *b && !isspace(*b); b++);
	for (; *b && isspace(*b); b++);
	for (e = b; *e && !isspace(*e); e++);
	*e = '\0';
	e++;
	gAddStr(dict, b, gNewWithLong(LongInteger, Atol(e)));
}

static	object	createResourceHeaderFile(char* infile, char *outfile)
{
	FILE	*ifp;
	FILE	*ofp;
	char	line[512];
	object	rval;

	if  (!(ifp = fopen(infile, "rt"))) {
		printf("\nCan't open input file [%s].\n\n", infile);
		return NULL;
	}
	if (*outfile) {
		if (!(ofp = fopen(outfile, "wt"))) {
			printf("\nCan't open output file [%s].\n\n", outfile);
			fclose(ifp);
			return NULL;
		}
	} else
		ofp = NULL;

	rval = gNew(StringDictionary);
	
	fgets(line, sizeof(line), ifp);
	while (!feof(ifp) && *line != '#')
		fgets(line, sizeof(line), ifp);

	while (!feof(ifp) && *line == '#') {
		*line = '(';
		line[strlen(line) - 1] = ')';
		if (ofp)
			fprintf(ofp, "%s\n", line);
		addEntryToDict(rval, line);
		fgets(line, sizeof(line), ifp);
	}
	if (ofp)
		fprintf(ofp, "\n");

	fclose(ifp);
	if (ofp)
		fclose(ofp);
	
	return rval;
}

static	void	get_header_file_name(FILE *fp, char *name)
{
	char	line[1024];

	*name = '\0';
	fgets(line, sizeof(line), fp);
	while (!*name && !feof(fp)) {
		line[8] = '\0';
		if (!strcmp(line, "#include")) {
			char	*p = strchr(line+9, '"');

			if (p) {
				strcpy(name, p+1);
				if (p = strrchr(name, '"'))
					*p = '\0';
			}
		}
		fgets(line, sizeof(line), fp);
	}
}

static	int	is_section(char* line, char *sect)
{
	char	buf[20];

	sprintf(buf, "// %s\n", sect);
	
	return !strcmp(line, buf);
}

static	int	end_of_section(char *line)
{
	return !strncmp(line, "////", 4) || *line == '#';
}

static	void	process_icons(FILE *ifp, FILE *cfp, FILE *sfp, object dict)
{
	char	line[1024];
	char	*p;
	object	obj;

	printf("\nProcessing icons");
	if (sfp) {
		fprintf(sfp, "\n  ; Process icons  **********************************************************\n");
		fprintf(sfp, "  (set! tobj1 (gNew IntegerDictionary))\n");
		fprintf(sfp, "  (gAddStr dict \"Icons\" tobj1)\n");
	}
	if (cfp) {
		fprintf(cfp, "\n\t// Process icons  **********************************************************\n");
		fprintf(cfp, "\n\tgAddStr(dict, \"Icons\", tobj1 = gNew(IntegerDictionary));\n");
	}
	fgets(line, sizeof(line), ifp);
	while (!feof(ifp) && !end_of_section(line)) {
		while (!isalpha(*line) && !end_of_section(line))
			fgets(line, sizeof(line), ifp);
		if (!end_of_section(line)) {
			if (p = strchr(line, ' ')) {
				*p = '\0';
				if (!(obj = gFindValueStr(dict, line)))
					printf("\nCan't find icon name: %s\n", line);
				if (sfp)
					fprintf(sfp, "  (gAddInt tobj1 %ld (gNewWithStr String \"%s\"))\n",
						gLongValue(obj), line);
				if (cfp)
					fprintf(cfp, "\tgAddInt(tobj1, %ld, gNewWithStr(String, \"%s\"));\n",
						gLongValue(obj), line);
			}
			fgets(line, sizeof(line), ifp);
		}
	}
}

static	int	process_control(FILE *ifp, FILE *cfp, FILE *sfp, char *dlg_name, char *line, int sz, object dict)
{
	char	*b = line + 20;
	char	*e;
	int	rval = 0;
	object	obj;

	if (*b == '"') {
		if (b[1] == '"')
			b += 3;
		else {
			b = strstr(b + 1, "\",");
			while (b && *(b - 1) == '"')
				b = strstr(b + 1, "\",");
			if (b)
				b += 2;
		}
	}
	if (b) {
		if (!(e = strchr(b, ','))) {
			fgets(line, sz, ifp);
			if (!feof(ifp)) {
				b = line + 20;
				e = strchr(b, ',');
			}
		}
		if (e) {
			*e = '\0';
			if (isalpha(*b) && strcmp(b, "IDC_STATIC") && strcmp(b, "-1") &&
			    strcmp(b, "IDOK") && strcmp(b, "IDCANCEL")) {
				rval = 1;
				if (!(obj = gFindValueStr(dict, b)))
					printf("\nCan't find control name: %s in dialog %s\n", line, dlg_name);
				if (sfp)
					fprintf(sfp, "  (gAddInt tobj2 %ld (gNewWithStr String \"%s\"))\n",
						gLongValue(obj), b);
				if (cfp)
					fprintf(cfp, "\tgAddInt(tobj2, %ld, gNewWithStr(String, \"%s\"));\n",
						gLongValue(obj), b);
			}
		}
	}
	return rval;
}

static 	void	process_this_dialog(FILE *ifp, FILE *cfp, FILE *sfp, char *dlg_name, object dict)
{
	char	line[1024];
	object	obj = gFindValueStr(dict, dlg_name);

	if (!obj)
		printf("\nCan't find dialog name: %s\n", dlg_name);
	if (sfp) {
		fprintf(sfp, "\n  (set! tobj3 (gNew LinkObject))\n");
		fprintf(sfp, "  (set! tobj2 (gNew IntegerDictionary))\n");
		fprintf(sfp, "  (gAddLast tobj3 (gNewWithStr String \"%s\"))\n", dlg_name);
		fprintf(sfp, "  (gAddLast tobj3 tobj2)\n");
		fprintf(sfp, "  (gAddInt tobj1 %ld tobj3)\n", gLongValue(obj));
	}
	if (cfp) {
		fprintf(cfp, "\n\ttobj3 = gNew(LinkObject);\n");
		fprintf(cfp, "\tgAddLast(tobj3, gNewWithStr(String, \"%s\"));\n", dlg_name);
		fprintf(cfp, "\tgAddLast(tobj3, tobj2 = gNew(IntegerDictionary));\n");
		fprintf(cfp, "\tgAddInt(tobj1, %ld, tobj3);\n", gLongValue(obj));
	}

	fgets(line, sizeof(line), ifp);
	while (!feof(ifp) && strcmp(line, "BEGIN\n"))
		fgets(line, sizeof(line), ifp);

	fgets(line, sizeof(line), ifp);
	while (!feof(ifp) && strcmp(line, "END\n")) {
		if (isalpha(line[4]))
			process_control(ifp, cfp, sfp, dlg_name, line, sizeof(line), dict);
		fgets(line, sizeof(line), ifp);
	}
}

static	void	process_dialogs(FILE *ifp, FILE *cfp, FILE *sfp, object dict)
{
	char	line[1024];
	char	*p;

	printf("\nProcessing dialogs");
	if (sfp) {
		fprintf(sfp, "\n  ; Process dialogs  ********************************************************\n");
		fprintf(sfp, "  (set! tobj1 (gNew IntegerDictionary))\n");
		fprintf(sfp, "  (gAddStr dict \"Dialogs\" tobj1)\n");
	}
	if (cfp) {
		fprintf(cfp, "\n\t// Process dialogs  ********************************************************\n");
		fprintf(cfp, "\n\tgAddStr(dict, \"Dialogs\", tobj1 = gNew(IntegerDictionary));\n");
	}
	fgets(line, sizeof(line), ifp);
	while (!feof(ifp) && !end_of_section(line)) {
		while (!isalpha(*line) && !end_of_section(line))
			fgets(line, sizeof(line), ifp);
		if (!end_of_section(line)) {
			if (p = strchr(line, ' ')) {
				*p = '\0';
				process_this_dialog(ifp, cfp, sfp, line, dict);
			}
			fgets(line, sizeof(line), ifp);
		}
	}
}

static	void	process_bitmaps(FILE *ifp, FILE *cfp, FILE *sfp, object dict)
{
	char	line[1024];
	char	*p;
	object	obj;

	printf("\nProcessing bitmaps");
	if (sfp) {
		fprintf(sfp, "\n  ; Process bitmaps  ********************************************************\n");
		fprintf(sfp, "  (set! tobj1 (gNew IntegerDictionary))\n");
		fprintf(sfp, "  (gAddStr dict \"Bitmaps\" tobj1)\n");
	}
	if (cfp) {
		fprintf(cfp, "\n\t// Process bitmaps  ********************************************************\n");
		fprintf(cfp, "\n\tgAddStr(dict, \"Bitmaps\", tobj1 = gNew(IntegerDictionary));\n");
	}
	fgets(line, sizeof(line), ifp);
	while (!feof(ifp) && !end_of_section(line)) {
		while (!isalpha(*line) && !end_of_section(line))
			fgets(line, sizeof(line), ifp);
		if (!end_of_section(line)) {
			if (p = strchr(line, ' ')) {
				*p = '\0';
				if (!(obj = gFindValueStr(dict, line)))
					printf("\nCan't find bitmap name: %s\n", line);
				if (sfp)
					fprintf(sfp, "  (gAddInt tobj1 %ld (gNewWithStr String \"%s\"))\n",
						gLongValue(obj), line);
				if (cfp)
					fprintf(cfp, "\tgAddInt(tobj1, %ld, gNewWithStr(String, \"%s\"));\n",
						gLongValue(obj), line);
			}
			fgets(line, sizeof(line), ifp);
		}
	}
}

static	int	process_menu_item(FILE *ifp, FILE *cfp, FILE *sfp, char *menu_name, char *line, int sz, object dict)
{
	char	*p;
	int	rval = 0;
	object	obj;

	if (p = strrchr(line, ',')) {
		for (p++; *p && !isalpha(*p); p++);

		if (!*p) {
			fgets(line, sz, ifp);
			for (p = line; *p && !isalpha(*p); p++);
		}
		if (*p) {
			p[strlen(p) - 1] = '\0';
			if (!(obj = gFindValueStr(dict, p)))
				printf("\nCan't find menu item name: %s in menu %s\n", p, menu_name);
			if (sfp)
				fprintf(sfp, "  (gAddInt tobj2 %ld (gNewWithStr String \"%s\"))\n",
					gLongValue(obj), p);
			if (cfp)
				fprintf(cfp, "\tgAddInt(tobj2, %ld, gNewWithStr(String, \"%s\"));\n",
					gLongValue(obj), p);
			rval = 1;
		}
	}
	return rval;
}

static 	void	process_this_menu(FILE *ifp, FILE *cfp, FILE *sfp, char *menu_name, object dict)
{
	char	line[1024];
	object	obj = gFindValueStr(dict, menu_name);
	
	if (!obj)
		printf("\nCan't find menu name: %s\n", menu_name);
	if (sfp) {
		fprintf(sfp, "\n  (set! tobj3 (gNew LinkObject))\n");
		fprintf(sfp, "  (set! tobj2 (gNew IntegerDictionary))\n");
		fprintf(sfp, "  (gAddLast tobj3 (gNewWithStr String \"%s\"))\n", menu_name);
		fprintf(sfp, "  (gAddLast tobj3 tobj2)\n");
		fprintf(sfp, "  (gAddInt tobj1 %ld tobj3)\n", gLongValue(obj));
	}
	if (cfp) {
		fprintf(cfp, "\n\ttobj3 = gNew(LinkObject);\n");
		fprintf(cfp, "\tgAddLast(tobj3, gNewWithStr(String, \"%s\"));\n", menu_name);
		fprintf(cfp, "\tgAddLast(tobj3, tobj2 = gNew(IntegerDictionary));\n");
		fprintf(cfp, "\tgAddInt(tobj1, %ld, tobj3);\n", gLongValue(obj));
	}
	
	fgets(line, sizeof(line), ifp);

	while (!feof(ifp) && strcmp(line, "END\n")) {
		if (strstr(line, "MENUITEM") && !strstr(line, "SEPARATOR"))
			process_menu_item(ifp, cfp, sfp, menu_name, line, sizeof(line), dict);
		fgets(line, sizeof(line), ifp);
	}
}

static	void	process_menus(FILE *ifp, FILE *cfp, FILE *sfp, object dict)
{
	char	line[1024];
	char	*p;

	printf("\nProcessing menus");
	if (sfp) {
		fprintf(sfp, "\n  ; Process menus  **********************************************************\n");
		fprintf(sfp, "  (set! tobj1 (gNew IntegerDictionary))\n");
		fprintf(sfp, "  (gAddStr dict \"Menus\" tobj1)\n");
	}
	if (cfp) {
		fprintf(cfp, "\n\t// Process menus  **********************************************************\n");
		fprintf(cfp, "\n\tgAddStr(dict, \"Menus\", tobj1 = gNew(IntegerDictionary));\n");
	}

	fgets(line, sizeof(line), ifp);
	while (!feof(ifp) && !end_of_section(line)) {
		while (!isalpha(*line) && !end_of_section(line))
			fgets(line, sizeof(line), ifp);
		if (!end_of_section(line)) {
			if (p = strchr(line, ' ')) {
				*p = '\0';
				process_this_menu(ifp, cfp, sfp, line, dict);
			}
			fgets(line, sizeof(line), ifp);
		}
	}
}

static	void	readResourceFile(char *rname, char* infile, char *c_outfile, char *scheme_outfile, char *scheme_header)
{
	FILE	*ifp = fopen(infile, "rt");

	if (ifp) {
		char	line[1024];
		object	dict;
		char	header_name[256];
		FILE	*cfp = *c_outfile ? fopen(c_outfile, "wt") : NULL;
		FILE	*sfp = *scheme_outfile ? fopen(scheme_outfile, "wt") : NULL;
		char	*p;

		*line = '\0';
		if (sfp) {
			fprintf(sfp, "(push-namespace 'ResourceLoad)\n\n");
			fprintf(sfp, "(let* [(tobj1 null)\n");
			fprintf(sfp, "       (tobj2 null)\n");
			fprintf(sfp, "       (tobj3 null)\n");
			fprintf(sfp, "       (dict  (gNew StringDictionary))]\n");
		}
		if (cfp) {
			fprintf(cfp, "#include \"generics.h\"\n\n");

			fprintf(cfp, "void\t%s_res_info(void)\n", Strlowc(rname));
			fprintf(cfp, "{\n");
			fprintf(cfp, "\tobject	tobj1;\n");
			fprintf(cfp, "\tobject	tobj2;\n");
			fprintf(cfp, "\tobject	tobj3;\n");
			fprintf(cfp, "\tobject	dict = gNew(StringDictionary);\n");
		}
		get_header_file_name(ifp, header_name);
		if (*header_name) {
			char	*p = strrchr(rname, '/');

			if (p) {
				strcpy(line, rname);
				p = strrchr(line, '/');
				*(p+1) = '\0';
				strcat(line, header_name);
				strcpy(header_name, line);
			}
				
			printf("\nProcessing resource header file");
			dict = createResourceHeaderFile(header_name, scheme_header);
		} else
			printf("\nCan't process resource header file\n");
		if (dict && (cfp || sfp))
			while (!feof(ifp)) {
				if (is_section(line, "Icon"))
					process_icons(ifp, cfp, sfp, dict);
				else if (is_section(line, "Dialog"))
					process_dialogs(ifp, cfp, sfp, dict);
				else if (is_section(line, "Bitmap"))
					process_bitmaps(ifp, cfp, sfp, dict);
				else if (is_section(line, "Menu"))
					process_menus(ifp, cfp, sfp, dict);
				fgets(line, sizeof(line), ifp);
			}
		strcpy(line, rname);
		Strlowc(line);
		if (p = strrchr(line, '/'))
			p++;
		else
			p = line;
		if (sfp) {
			fprintf(sfp, "\n  (gAddGlobal Application \"ResourceInformation-%s\" dict))\n\n", p);
			fprintf(sfp, "(pop-namespace)\n");
			fprintf(sfp, "(remove-namespace 'ResourceLoad)\n");
		}
		if (cfp) {
			fprintf(cfp, "\n\tgAddGlobal(Application, \"ResourceInformation-%s\", dict);\n", p);
			fprintf(cfp, "}\n");
		}

		if (dict)
			gDeepDispose(dict);
		if (cfp)
			fclose(cfp);
		if (sfp)
			fclose(sfp);
	} else
		printf("\nCan't open resource file: %s\n\n", infile);
	
	if (ifp)
		fclose(ifp);
	printf("\n");
}

static	void	processFile(char* rcname, int mode)
{
	char	fname_no_ext[256];
	char	scheme_header[256];
	char	scheme_controls[256];
	char	c_controls[256];
	char	*p;

	strcpy(fname_no_ext, rcname);
	if (p = strrchr(fname_no_ext, '.'))
	    *p = '\0';
	if (mode & MODE_HFILE)
		sprintf(scheme_header, "%s_RC_Include.scm", fname_no_ext);
	else
		*scheme_header = '\0';
	if (mode & MODE_SFILE)
		sprintf(scheme_controls, "%s_RC_Data.scm", fname_no_ext);
	else
		*scheme_controls = '\0';
	if (mode & MODE_CFILE)
		sprintf(c_controls, "%s_RC_Data.c", fname_no_ext);
	else
		*c_controls = '\0';

	printf("\nProcessing %s", rcname);
	readResourceFile(fname_no_ext, rcname, c_controls, scheme_controls, scheme_header);
}

main(int argc, char *argv[])
{
	InitDynace(&argc);
	Application;	// Cause console32.lib to link in
	
	if (argc < 2)
		usage();
	else {
		int	mode = 0;
		int	i;
		
		for (i = 2; i < argc; i++) {
			if (!strcmp(argv[i], "-c"))
				mode += MODE_CFILE;
			else if (!strcmp(argv[i], "-s"))
				mode += MODE_SFILE;
			else if (!strcmp(argv[i], "-h"))
				mode += MODE_HFILE;
		}
		if (!mode)
			mode = MODE_CFILE;
		processFile(argv[1], mode);
	}
	return 0;
}
