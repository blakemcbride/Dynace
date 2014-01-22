

#include "generics.h"
#include "../Windows/ctlsave.h"


static	void	staticTextControl(FILE *fp, object ctl);
static	void	textControl(FILE *fp, object ctl);
static	void	numericControl(FILE *fp, object ctl);
static	void	dateControl(FILE *fp, object ctl);
static	void	pushButtonControl(FILE *fp, object ctl);
static	void	radioButtonControl(FILE *fp, object ctl);
static	void	checkboxControl(FILE *fp, object ctl);
static	void	listboxControl(FILE *fp, object ctl);
static	void	comboboxControl(FILE *fp, object ctl);
static	void	rectControl(FILE *fp, object ctl);
static	void	lineControl(FILE *fp, object ctl);
void	saveFlashDataFile(object wind, char *name); 

static	int	height(int v);
static	int	width(int v);
static	int	xPos(int v);
static	int	yPos(int v);
static	void	parse_font_name(char *res, char *font, int *bold, int *italic);
static	int	setFont(FILE *fp, object ctl, char *name);


extern float SCALE_FACTOR;

char dialogName[256];

void setDialogName(char *name)
{
	int l=0;

	while (*name)
	{
		dialogName[l]=*name;

		if (dialogName[l]=='-')
			dialogName[l]='_';

		l++;

		name++;
	}
	dialogName[l]=0;
}



void	saveFlashFile(object wind, char *fname, char *name)
{
	FILE	*fp = fopen(fname, "wt");
	object	ctllist = gGetControls(wind);
	object	seq, ctl, cls;

	if (!fp)
		return;

	setDialogName(name);

	fprintf(fp, "\nimport com.integra.system.*;\n\n");
	fprintf(fp, "import com.integra.services.Service;\n");
	fprintf(fp, "import com.integra.services.ServiceResponse;\n\n");
	fprintf(fp, "\nvar  ScalingFactor = %f;\n\n",SCALE_FACTOR);
	fprintf(fp, "var  dlg%s:Dialog = new Dialog();\n",dialogName);
	fprintf(fp, "var  ctl:Object;\n\n");
	if (ctllist)
		for (seq = gSequence(ctllist) ; ctl = gNext(seq) ; ) {
			cls = ClassOf(ctl);
			if (cls == StaticTextControl)
				staticTextControl(fp, ctl);
			else if (cls == TextControl)
				textControl(fp, ctl);
			else if (cls == NumericControl)
				numericControl(fp, ctl);
			else if (cls == DateControl)
				dateControl(fp, ctl);
			else if (cls == PushButton)
				pushButtonControl(fp, ctl);
			else if (cls == RadioButton)
				radioButtonControl(fp, ctl);
			else if (cls == CheckBox)
				checkboxControl(fp, ctl);
			else if (cls == ListBox)
				listboxControl(fp, ctl);
			else if (cls == ComboBox)
				comboboxControl(fp, ctl);
			else if (cls == RectControl)
				rectControl(fp, ctl);
			else if (cls == LineControl)
				lineControl(fp, ctl);
		}
	fprintf(fp, "\n#include \"%s_d.as\"\n", name);
	fprintf(fp, "\n#include \"%s.as\"\n", name);
	fclose(fp);
}

char * stripChoicePathOut(char *choicepath, char *path)
{
	char dataset[512];
	char *ret=malloc(strlen(path)+1);
	ret[0]=0;

	//chop dataset out of equation

	strcpy(dataset,choicepath);

	if (strstr(dataset,":"))
		*strstr(dataset,":")=0;
	else
		dataset[0]=0;

	if (strstr(choicepath,":"))
	{
		char t[512];
		strcpy(t,choicepath);
		*strstr(t,":")=0;

		choicepath+=strlen(t)+2;
	}
	
	//now see if path starts with choicepath
	if (strstr(path,choicepath))
	{
		path+=strlen(choicepath);

	}

	if (*path=='/')
		path++;

	strcat(ret,path);

	return ret;
}

void	saveFlashDataFile(object wind, char *name)
{
	char fname[512];
	object	ctllist = gGetControls(wind);
	object	seq, ctl, cls;
	FILE	*fp;

	sprintf(fname,"%s_d.as",name);
	setDialogName(name);

	if (!access(fname,0)) {
		if (IDYES!=gQuery(wind,"WARNING",
				"The data binding file already exists.\nDo you want to overwrite it?",MB_YESNO))
			return;
	}

	fp = fopen(fname, "wt");
	
	if (!fp)
		return;

	if (ctllist)
		for (seq = gSequence(ctllist) ; ctl = gNext(seq) ; ) {
			char *name=gName(ctl);

			if (!strcmp("STATIC",name) || !strcmp("LINE",name) || !strcmp("RECT",name))
				continue;

			cls = ClassOf(ctl);
			if (cls == ComboBox || cls == ListBox )
			{
				char *choicepath=gChoiceXPathBinding(ctl);
				char *mainkeypath=gXPathBinding(ctl);
				char *listkeypath=stripChoicePathOut(choicepath,gListKeyXPathBinding(ctl));
				char *datapath=stripChoicePathOut(choicepath,gDataXPathBinding(ctl));
				char *textpath=stripChoicePathOut(choicepath,gTextXPathBinding(ctl));


				fprintf(fp, "ctl = dlg%s.getControl(\"%s\");\n",dialogName,name);


				fprintf(fp, "ctl.mainkeypath = \"%s\";\n",mainkeypath);
				fprintf(fp, "ctl.choicepath = \"%s\";\n",choicepath);
				fprintf(fp, "ctl.listkeypath = \"%s\";\n",listkeypath);
				fprintf(fp, "ctl.datapath = \"%s\";\n",datapath);
				fprintf(fp, "ctl.textpath = \"%s\";\n",textpath);


				free(listkeypath);
				free(datapath);
				free(textpath);
			}
			else
				fprintf(fp, "dlg%s.bind(\"%s\", \"%s\");\n",dialogName,name,gXPathBinding(ctl));
		}

	fclose(fp);
}

static	void	staticTextControl(FILE *fp, object ctl)
{
	CTLTYPE_STATIC_t	v;
	char	*cp, name[81];
	static	int	id = 1;
	int	size;
	
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "STATIC"))
		return;

	if (!strcmp(v.name, "STATIC"))
		sprintf(name, "staticText%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.Label, \"%s\", getNextHighestDepth(), {});\n", name);
	size = setFont(fp, ctl, name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, (int)(v.xPos+size*.54-1), v.yPos-2);
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, v.width, v.height+4);

	if (v.right == 'Y')
		fprintf(fp, "%s.setStyle(\"textAlign\", \"%s\");\n", name, "right");
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
/*
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
*/
	fprintf(fp, "ctl = dlg%s.newControl(StaticTextControl, %s, \"%s\");\n", dialogName, name, name);
/*
	cp = gStringValue(ctl);
	fprintf(fp, "%s.text = \"%s\";\n", name, cp?cp:"");
*/
	fprintf(fp, "\n");
}

static	void	textControl(FILE *fp, object ctl)
{
	CTLTYPE_TEXT_t	v;
	char	*cp, name[81];
	static	int	id = 1;
	object	dobj;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Text"))
		sprintf(name, "textControl%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.TextInput, \"%s\", getNextHighestDepth(), {enabled: false, _alpha: 30});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos), yPos(v.yPos));
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, width(v.width), 2+height(v.height));
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
	dobj = gGetDefault(ctl);
	if (dobj) {
		cp = gStringValue(ctl);
		if (cp  &&  *cp  &&  strcmp(cp, "Text"))
			fprintf(fp, "%s.text = \"%s\";\n", name, cp);
	}

	fprintf(fp, "ctl = dlg%s.newControl(TextControl, %s, \"%s\");\n", dialogName, name, name);
	setFont(fp, ctl, "ctl");
	fprintf(fp, "\n");
}

static	void	numericControl(FILE *fp, object ctl)
{
	CTLTYPE_NUMERIC_t	v;
	char	*cp, name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Numeric"))
		sprintf(name, "numericControl%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.TextInput, \"%s\", getNextHighestDepth(), {enabled: false, _alpha: 30});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos), yPos(v.yPos));
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, width(v.width), height(v.height));
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
	fprintf(fp, "ctl = dlg%s.newControl(NumericControl, %s, \"%s\");\n", dialogName, name, name);
	setFont(fp, ctl, "ctl");
	fprintf(fp, "ctl.setNumericRange(%f, %f, %d);\n", v.minimum, v.maximum, v.dp);
	fprintf(fp, "ctl.setMask(\"%s\");\n", v.format);
	fprintf(fp, "\n");
}

static	void	dateControl(FILE *fp, object ctl)
{
	CTLTYPE_DATE_t	v;
	char	*cp, name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Date"))
		sprintf(name, "dateControl%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.TextInput, \"%s\", getNextHighestDepth(), {enabled: false, _alpha: 30});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos), yPos(v.yPos));
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, width(v.width), height(v.height));
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
	fprintf(fp, "ctl = dlg%s.newControl(DateControl, %s, \"%s\");\n", dialogName, name, name);
	setFont(fp, ctl, "ctl");
	fprintf(fp, "\n");
}

static	void	pushButtonControl(FILE *fp, object ctl)
{
	CTLTYPE_PUSHBUTTON_t	v;
	char	*cp, name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Push"))
		sprintf(name, "pushButtonControl%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.Button, \"%s\", getNextHighestDepth(), {enabled: false, _alpha: 30});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos), yPos(v.yPos)+2);
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, width(v.width)-2, height(v.height)-2);
	cp = gStringValue(ctl);
	if (cp  &&  *cp)
		fprintf(fp, "%s.label = \"%s\";\n", name, cp);
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
	fprintf(fp, "ctl = dlg%s.newControl(PushButtonControl, %s, \"%s\");\n", dialogName, name, name);
	setFont(fp, ctl, "ctl");
	fprintf(fp, "\n");
}

static	void	radioButtonControl(FILE *fp, object ctl)
{
	CTLTYPE_RADIOBUTTON_t	v;
	char	*cp, name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Radio"))
		sprintf(name, "radioButtonControl%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.RadioButton, \"%s\", getNextHighestDepth(), {enabled: false, _alpha: 30});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos), yPos(v.yPos));
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, width(v.width), height(v.height));
	cp = gGetTitle(ctl);
	if (cp  &&  *cp)
		fprintf(fp, "%s.label = \"%s\";\n", name, cp);
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
	fprintf(fp, "ctl = dlg%s.newControl(RadioButtonControl, %s, \"%s\");\n", dialogName, name, name);
	setFont(fp, ctl, "ctl");
	fprintf(fp, "\n");
}

static	void	checkboxControl(FILE *fp, object ctl)
{
	CTLTYPE_CHECKBOX_t	v;
	char	*cp, name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Check"))
		sprintf(name, "checkboxControl%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.CheckBox, \"%s\", getNextHighestDepth(), {enabled: false, _alpha: 30});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos)+2, yPos(v.yPos));
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, width(v.width), height(v.height));
	cp = gGetTitle(ctl);
	if (cp  &&  *cp)
		fprintf(fp, "%s.label = \"%s\";\n", name, cp);
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
	fprintf(fp, "ctl = dlg%s.newControl(CheckboxControl, %s, \"%s\");\n", dialogName, name, name);
	setFont(fp, ctl, "ctl");
	fprintf(fp, "\n");
}

static	void	listboxControl(FILE *fp, object ctl)
{
	CTLTYPE_LISTBOX_t	v;
	char	name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "List"))
		sprintf(name, "listboxControl%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.List, \"%s\", getNextHighestDepth(), {enabled: false, _alpha: 30});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos), yPos(v.yPos));
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, width(v.width), height(v.height));
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
	fprintf(fp, "ctl = dlg%s.newControl(ListboxControl, %s, \"%s\");\n", dialogName, name, name);
	setFont(fp, ctl, "ctl");
	fprintf(fp, "\n");
}

static	void	comboboxControl(FILE *fp, object ctl)
{
	CTLTYPE_COMBOBOX_t	v;
	char	name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Combo"))
		sprintf(name, "comboboxControl%d", id++);
	else
		strcpy(name, v.name);
/*
	fprintf(fp, "createClassObject(mx.controls.ComboBox, \"%s\", getNextHighestDepth(), {enabled: false, _alpha: 30});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos)+2, yPos(v.yPos)+2);
	fprintf(fp, "%s.setSize(%d * ScalingFactor, 20);\n", name, width(v.width)-4);
*/
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
	fprintf(fp, "ctl = dlg%s.newControl(ComboboxControl, %s, \"%s\");\n", dialogName, name, name);
	setFont(fp, ctl, "ctl");
	fprintf(fp, "\n");
}

static	void	rectControl(FILE *fp, object ctl)
{
	CTLTYPE_RECT_t	v;
	char	*cp, name[81];
	static	int	id = 1;

	return;

	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "STATIC"))
		sprintf(name, "staticText%d", id++);
	else
		strcpy(name, v.name);
	fprintf(fp, "createClassObject(mx.controls.Label, \"%s\", getNextHighestDepth(), {});\n", name);
	fprintf(fp, "%s.move(%d * ScalingFactor, %d * ScalingFactor);\n", name, xPos(v.xPos), yPos(v.yPos));
	fprintf(fp, "%s.setSize(%d * ScalingFactor, %d * ScalingFactor);\n", name, width(v.width), height(v.height));
	fprintf(fp, "box(%d, %d, %d, %d);\n", yPos(v.yPos), xPos(v.xPos), width(v.width), height(v.height));
	setFont(fp, ctl, name);

	if (v.DT_Format & DT_CENTER)
		fprintf(fp, "%s.setStyle(\"textAlign\", \"%s\");\n", name, "center");
	else if (v.DT_Format & DT_RIGHT)
		fprintf(fp, "%s.setStyle(\"textAlign\", \"%s\");\n", name, "right");
	if (v.hidden == 'Y')
		fprintf(fp, "%s.visible = false;\n", name);
/*
	if (v.disabled == 'Y')
		fprintf(fp, "%s.enabled = false;\n", name);
*/
	cp = gStringValue(ctl);
	fprintf(fp, "%s.text = \"%s\";\n", name, cp?cp:"");
	fprintf(fp, "\n");
}

static	void	lineControl(FILE *fp, object ctl)
{
	CTLTYPE_LINE_t	v;
	char	*cp, name[81];
	static	int	id = 1;

	return;

	
	gGetControlParameters(ctl, &v);
	if (v.width > v.height)
		fprintf(fp, "line(%d, %d, %d, %d);\n", v.yPos, v.xPos, v.width, v.height);		//  horz
	else
		fprintf(fp, "line(%d, %d, %d, %d);\n", v.yPos, v.xPos, v.width, v.height); 		//  vert

	fprintf(fp, "\n");
}


static	int	height(int v)
{
	return v/*+2*/;
}

static	int	width(int v)
{
	return v/*+3*/;
}

static	int	xPos(int v)
{
	return v/*-2*/;
}

static	int	yPos(int v)
{
	return v/*-2*/;
}

static	void	parse_font_name(char *res, char *font, int *bold, int *italic)
{
	char *fontSave=res;

	*bold = *italic = 0;

	

	while (*font)
		if (*font == ' '  &&  !strncmp(font, " Bold", 5)  &&  (!font[5]  ||  font[5] == ' ')) {
			*bold = 1;
			font += 5;
		} else if (*font == ' '  &&  !strncmp(font, " Italic", 7)  &&  (!font[7]  ||  font[7] == ' ')) {
			*italic = 1;
			font += 7;
		} else
			*res++ = *font++;
	*res = '\0';

	if (!strcmp(fontSave, "MS Sans Serif"))
		strcpy(fontSave, "Microsoft Sans Serif");
}

static	int	setFont(FILE *fp, object ctl, char *name)
{
	object	font;
	int	bold, italic;
	char	fntName[80];

	font = gGetFont(ctl);
	parse_font_name(fntName, gName(font), &bold, &italic);
	fprintf(fp, "%s.setStyle(\"fontFamily\", \"%s\");\n", name, fntName);
	fprintf(fp, "%s.setStyle(\"fontSize\", %f * ScalingFactor);\n", name, gPointSize(font)* 1.0/* *1.3*/);
	if (italic)
		fprintf(fp, "%s.setStyle(\"fontStyle\", \"italic\");\n", name);
	if (bold)
		fprintf(fp, "%s.setStyle(\"fontWeight\", \"bold\");\n", name);
	return gPointSize(font);
}
