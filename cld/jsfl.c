

#include "generics.h"
#include "../Windows/ctlsave.h"
#include "../Windows/color.h"



static	void	staticText(FILE *fp, object ctl);
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
static	void	imageControl(FILE *fp, object ctl);
static	void	windowSize(FILE *fp, object ctl);


static	void	parse_font_name(char *res, char *font, int *bold, int *italic);
static	int	setFont(FILE *fp, object ctl, char *name);


float SCALE_FACTOR = 1.3333;

int docHeight;
int docWidth;

#define EDGE_BUFFER_SIZE 10

char *Getenv(char *x)
{
	return "";
}

void	saveJSFLFile(object wind, char *fname, char *name)
{
	FILE	*fp = fopen(fname, "wt");
	object	ctllist = gGetControls(wind);
	object	seq, ctl, cls;

	if (!fp)
		return;

	docHeight=640;
	docWidth=480;
	
	fprintf(fp,"if (fl.documents.length < 1)\n");
	fprintf(fp, "\tfl.createDocument();\n");


	fprintf(fp, "\nfl.runScript(\"file:///com/integra/system/utils.jsfl\");\n");
	

	fprintf(fp, "var doc=fl.getDocumentDOM();\n\n\n");

	fprintf(fp,"\n\ndoc.getTimeline().layers[0].frames[0].actionScript = '#include \"%s_i.as\"';\n\n", name);
	
	if (ctllist)
		for (seq = gSequence(ctllist) ; ctl = gNext(seq) ; ) {
			cls = ClassOf(ctl);
			if (cls == StaticTextControl)
				staticText(fp, ctl);
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
				if (!strcmp(gName(ctl),"WINDBORDERRECT"))
					windowSize(fp, ctl);
				else 
					rectControl(fp, ctl);
			else if (cls == LineControl)
				lineControl(fp, ctl);
			else if (cls == ImageControl)
				imageControl(fp, ctl);

			fprintf(fp,"doc.selectNone();\n");
		}

	fprintf(fp,"\nfl.componentsPanel.addItemToDocument({x:0, y:0}, 'User Interface', 'Alert');");
	fprintf(fp,"\ndoc.deleteSelection();");
	fprintf(fp,"\ndoc.height=%d;",docHeight);
	fprintf(fp,"\ndoc.width=%d;",docWidth);
//	fprintf(fp,"\n\ndoc.getTimeline().layers[0].frames[0].actionScript = '#include \"%s_i.as\"';\n\n", name);
//	fprintf(fp,"\n\ndoc.testMovie();\n\n");
	fprintf(fp,"\n\nfl.saveDocument(doc,'file:///%s.fla');\n\n",name);
	fprintf(fp,"\n\nfl.revertDocument(doc);\n\n");


	fclose(fp);
}


static char * escapeQuotes(char * cp)
{
	char *cp2=malloc(strlen(cp)*2+1);
	char *cp2start=cp2;

	ZeroMemory(cp2,strlen(cp)*2+1);

	while (*cp)
	{
		if (*cp=='"')
		{
			*cp2='\\';
			cp2++;
		}
		*cp2=*cp;
		cp2++;
		cp++;
	}
	cp2=0;
	return cp2start;
}

static char *getEscapedTitle(object ctl)
{
	char *cp = gGetTitle(ctl);
	
	if (cp)
	{
		char *cp2=escapeQuotes(cp);

	//	free(cp);

		return cp2;
	}
	else
	{
		return NULL;
	}

}

static char *getEscapedStringValue(object ctl)
{
	char *cp = gStringValue(ctl);
	if (cp)
	{
		char *cp2=escapeQuotes(cp);

	//	free(cp);

		return cp2;
	}
	else
	{
		return NULL;
	}
}
static	void	fixScreenSize(int height, int width)
{
	docHeight=(height>docHeight)?height:docHeight;
	docWidth=(width>docWidth)?width:docWidth;

}
static	void	staticText(FILE *fp, object ctl)
{
	CTLTYPE_STATIC_t	v;
	char	*cp, name[81], fntName[80];
	static	int	id = 1;
	int	size, bold, italic;
	object	font;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "STATIC"))
		sprintf(name, "staticText%d", id++);
	else
		strcpy(name, v.name);
	font = gGetFont(ctl);
	parse_font_name(fntName, gName(font), &bold, &italic);
	size = gPointSize(font);
	cp = getEscapedStringValue(ctl);
	fprintf(fp, "doc.addText(\"%s\", %f, \"%s\", %s, \"%s\", %d, %d, %d, %d);\n", cp?cp:"", size*SCALE_FACTOR, fntName, bold?"true":"false", v.right=='Y'?"right":"left",
		(int)(v.xPos+size*.54-1), v.yPos/*-2*/,
		(int)(v.width+size*.54-1), v.height/*+4*/);
	fprintf(fp, "\n");
	if (strcmp(v.name, "STATIC"))
		fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	

	//fprintf(fp, "doc.selection[0].parameters['html'].value = \"%s\";\n", (gHTMLFlag(ctl))?"true":"false");

	if (gURL(ctl))
	{
		fprintf(fp, "fl.getDocumentDOM().setElementProperty('textType', 'static');\n");
		fprintf(fp, "fl.getDocumentDOM().setElementTextAttr('url', '%s');\n",gURL(ctl));
		fprintf(fp, "fl.getDocumentDOM().setElementProperty('textType', 'dynamic');");
	}
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
}

static	void	textControl(FILE *fp, object ctl)
{
	CTLTYPE_TEXT_t	v;
	char	name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Text"))
		sprintf(name, "textControl%d", id++);
	else
		strcpy(name, v.name);
//	fprintf(fp, "doc.createClassObject(\"ITextInput\", \"%s\");\n", name);
	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'Standard Components', 'ITextInput');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	
	fprintf(fp, "doc.selection[0].parameters['password'].value = \"%s\";\n", (gPasswordFlag(ctl))?"true":"false");
	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, v.height);
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
}

static	void	numericControl(FILE *fp, object ctl)
{
	CTLTYPE_NUMERIC_t	v;
	char	name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Numeric"))
		sprintf(name, "numericControl%d", id++);
	else
		strcpy(name, v.name);
//	fprintf(fp, "doc.createClassObject(\"ITextInput\", \"%s\");\n", name);
	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'Standard Components', 'ITextInput');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	
	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, v.height);
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
}

static	void	dateControl(FILE *fp, object ctl)
{
	CTLTYPE_DATE_t	v;
	char	name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "Date"))
		sprintf(name, "dateControl%d", id++);
	else
		strcpy(name, v.name);

	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'Standard Components', 'ITextInput');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	
	//fprintf(fp, "doc.createClassObject(\"ITextInput\", \"%s\");\n", name);
	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, v.height);
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
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
//	fprintf(fp, "doc.createClassObject(\"Button\", \"%s\");\n", name);
	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'Standard Components', 'IButton');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	

	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, v.height);
	cp = getEscapedStringValue(ctl);
	if (cp  &&  *cp)
		fprintf(fp, "doc.selection[0].parameters['label'].value = \"%s\";\n", cp);
	else
		fprintf(fp, "doc.selection[0].parameters['label'].value = \"\";\n");
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
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
	//fprintf(fp, "doc.createClassObject(\"RadioButton\", \"%s\");\n", name);
	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'Standard Components', 'IRadioButton');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	
	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, v.height);
	cp = getEscapedTitle(ctl);
	if (cp  &&  *cp)
		fprintf(fp, "doc.selection[0].parameters['label'].value = \"%s\";\n", cp);
	else
		fprintf(fp, "doc.selection[0].parameters['label'].value = \"\";\n");
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
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
	//fprintf(fp, "doc.createClassObject(\"CheckBox\", \"%s\");\n", name);
	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'Standard Components', 'ICheckBox');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	
	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, v.height);
	cp = getEscapedTitle(ctl);
	if (cp  &&  *cp)
		fprintf(fp, "doc.selection[0].parameters['label'].value = \"%s\";\n", cp);
	else
		fprintf(fp, "doc.selection[0].parameters['label'].value = \"\";\n");

	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
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
	//fprintf(fp, "doc.createClassObject(\"List\", \"%s\");\n", name);
	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'Standard Components', 'IList');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	
	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, v.height);
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
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
	//fprintf(fp, "doc.createClassObject(\"ComboBox\", \"%s\");\n", name);
	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'Standard Components', 'IComboBox');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	
	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, 20);
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
}


static const char * flashColor(int winColor)
{

	
	switch (winColor)
	{
		case CLR_Black :	return "#000000";
			
		case CLR_Orange    : 	return "#FF8000";
			
		case CLR_Aqua   :	return "#000000";
			
		case CLR_Olive     :	return "#FFFF00";
			
		case CLR_Fuchsia  :  	return "#FF00FF";
			
		case CLR_Gray  :  	return "#808080";
			
		case CLR_Lime      :	return "#00FF00";
			
		case CLR_Magenta      :	return "#FF00FF";
			
		case CLR_Maroon     :	return "#800000";
			
		case CLR_Navy       :	return "#000080";
			
		case CLR_Green     :	return "#00FF00";
			
		case CLR_Purple    :	return "#800080";
			
		case CLR_Blue      :	return "#0000FF";
			
		case CLR_Silver   :	return "#C0C0C0";
			
		case CLR_Yellow      :	return "#FFFF00";
			
		case CLR_Red      :	return "#FF0000";
			
		case CLR_White     : return "#FFFFFF";
							
		case CLR_Teal      :	return "#008080";
			
		case CLR_Violet     : return "#800080";
							
		default:
			return "#000000";

	}

}

static	void	windowSize(FILE *fp, object ctl)
{
	CTLTYPE_RECT_t	v;
	gGetControlParameters(ctl, &v);
	docHeight=v.height;
	docWidth=v.width;
}

static	void	rectControl(FILE *fp, object ctl)
{
	CTLTYPE_RECT_t	v;
	object	font;
	char	*cp, name[81], fntName[80], *just;
	static	int	id = 1;
	int	size, bold, italic;

	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "STATIC"))
		sprintf(name, "staticText%d", id++);
	else
		strcpy(name, v.name);
	font = gGetFont(ctl);
	parse_font_name(fntName, gName(font), &bold, &italic);
	size = gPointSize(font);
	cp = getEscapedStringValue(ctl);
	if (v.DT_Format & DT_CENTER)
		just = "center";
	else if (v.DT_Format & DT_RIGHT)
		just = "right";
	else
		just = "left";



	fprintf(fp, "doc.addText(\"%s\", %f, \"%s\", %s, \"%s\", %d, %d, %d, %d);\n", cp?cp:"", size*SCALE_FACTOR, fntName, bold?"true":"false", just, v.xPos, v.yPos, 
		v.width, v.height);
	fprintf(fp, "doc.box(%d, %d, %d, %d, %d, \"%s\");\n",  v.xPos,v.yPos, v.width, v.height,v.frameThickness,flashColor(v.frameColor));
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
}

static	void	imageControl(FILE *fp, object ctl)
{
	CTLTYPE_IMAGE_t	v;
	char	*p,*p2;
	char	*cp, name[81];
	static	int	id = 1;
	int loop;

	gGetControlParameters(ctl, &v);

	if (!strcmp(v.name, "BITMAP"))
		sprintf(name, "imageControl%d", id++);
	else
		strcpy(name, v.name);

	fprintf(fp, "fl.componentsPanel.addItemToDocument({x:%d, y:%d}, 'User Interface', 'Loader');\n",v.xPos, v.yPos);
	fprintf(fp, "doc.setElementProperty('name','%s');\n",name);	
	fprintf(fp, "doc.setSize(%d, %d, %d, %d);\n", v.xPos, v.yPos, v.width, v.height);

	p=gStringValue(ctl);
	p2=malloc(strlen(p)+1);
	strcpy(p2,p);
	for (loop=0;loop<strlen(p2);loop++)
		if (p2[loop]=='\\')
			p2[loop]='/';
	fprintf(fp, "doc.selection[0].parameters['contentPath'].value = \"%s\";\n", p2);
	fprintf(fp, "doc.selection[0].parameters['scaleContent'].value = \"false\";\n");
	free(p2);

	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
}


static	void	lineControl(FILE *fp, object ctl)
{
	CTLTYPE_LINE_t	v;
	char	*cp, name[81];
	static	int	id = 1;
	gGetControlParameters(ctl, &v);
	if (v.width > v.height)
		fprintf(fp, "doc.line(%d, %d, %d, %d, \"%s\");\n", v.yPos, v.xPos, v.width, v.height,flashColor(v.lineColor));		//  horz
	else
		fprintf(fp, "doc.line(%d, %d, %d, %d, \"%s\");\n", v.yPos, v.xPos, v.width, v.height,flashColor(v.lineColor)); 		//  vert
	fprintf(fp, "\n");
	fixScreenSize(v.height+v.yPos+EDGE_BUFFER_SIZE,v.width+v.xPos+EDGE_BUFFER_SIZE);
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

