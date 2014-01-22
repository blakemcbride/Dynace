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

#include "logfile.h"
#include "hdlcache.h"
#include "ctlsave.h"

#include <ctype.h>

#include <windows.h>
#include <stdlib.h>


#ifndef	INT
#define	INT	int
#endif

#define PALVERSION      0x300
#define MAXPALETTE      256       /* max. # supported palette entries */

/* macro to determine if resource is a DIB */
#define ISDIB(bft) ((bft) == BFT_BITMAP)

/* Macro to determine to round off the given value to the closest byte */
#define WIDTHBYTES(i)   ((i+31)/32*4)
#define MAXREAD  32768                 /* Number of bytes to be read during */
                                       /* each read operation.              */
#define BFT_BITMAP 0x4d42   /* 'BM' */

#define	StartWait()
#define	EndWait()


// the class can be used to load other image files, such as gif and jpeg files with minor changes.
// therefore the class is named ImageControl.

defclass	GenericControl  : ImageControl  {
	object	iProperties;
	object	iTypeName;
};



cvmeth	vNew(UINT ctlID, char *name, dlg)
{
	object	obj = vNew(super, ctlID, name, dlg);
	ivType	*iv = ivPtr(obj);

	iProperties = gNew(StringDictionary);

	return obj;
}


cmeth	gNewWindowControl(UINT ctlID, char *name, parent)
{
	object	obj = gNewWindowControl(super, ctlID, name, parent);
	ivType	*iv = ivPtr(obj);

	iProperties = gNew(StringDictionary);
	
	return obj;
}



imeth	object	gDispose, gDeepDispose ()
{
	if (iProperties)
		gDispose(iProperties);
	if (iTypeName)
		gDispose(iTypeName);
	return gDispose(super);
}

#define	BUFLEN	128


cmeth	gCLDPasteControl(FILE *fp, parent, short nXshift, short nYshift)
{
	return gCLDPasteControl(super, fp,parent,nXshift,nYshift);

}




imeth gWriteXML(FILE *fp)
{

	CTLTYPE_IMAGE_t	v;
	char			buf[1024];
	object			fnt = gGetFont(self);
	object			seq,sit;

	gGetControlParameters(self, &v);
	
	if (!*gStringValue(self))
		return self;

	fprintf(fp,"\t\t<%s>\n",gStringValue(self));
	fprintf(fp,"\t\t\t<name>%s</name>\n",gStringToXML(XMLNode,buf,v.name));
	fprintf(fp,"\t\t\t<x>%d</x>\n",v.xPos);
	fprintf(fp,"\t\t\t<y>%d</y>\n",v.yPos);
	fprintf(fp,"\t\t\t<width>%d</width>\n",v.width);
	fprintf(fp,"\t\t\t<height>%d</height>\n",v.height);
	fprintf(fp,"\t\t\t<hidden>%c</hidden>\n",v.hidden);
	fprintf(fp,"\t\t\t<disabled>%c</disabled>\n",v.disabled);
	fprintf(fp,"\t\t\t<helpTopicLen>%d</helpTopicLen>\n",v.helpTopicLen);
	fprintf(fp,"\t\t\t<helpTopic>%s</helpTopic>\n",gStringToXML(XMLNode,buf,gGetTopic(self)));

	for (seq=gSequence(iProperties) ; IsObj(seq) && ( sit = gNext(seq)) ; )
	{
		object key=gKey(sit);
		object val=gValue(sit);
		char *keyd=gStringValue(key);
		char *vald=gStringValue(val);
		
		fprintf(fp,"\t\t\t<%s>%s</%s>\n",gStringValue(gKey(sit)),gStringValue(gValue(sit)),gStringValue(gKey(sit)));
	}
	fprintf(fp,"\t\t</%s>\n",gStringValue(self));


	return self;
}


cmeth gLoadControlFromXML(curnode,parent)
{
	CTLTYPE_IMAGE_t	v;
	int             end;
	object          ctl;
	short           size;
	char            *p, buf[BUFLEN];
	double          controlScaleFactor;
	object			node;
	
	memset(&v, 0, sizeof v);
	
	gPopulateStringFromNode(curnode,v.name,"name");
	v.xPos=gGetIntFromNode(curnode,"x");
	v.yPos=gGetIntFromNode(curnode,"y");
	v.width=gGetIntFromNode(curnode,"width");
	v.height=gGetIntFromNode(curnode,"height");
	v.hidden=gGetCharFromNode(curnode,"hidden");
	v.disabled=gGetCharFromNode(curnode,"disabled");
	v.helpTopicLen=gGetIntFromNode(curnode,"helpTopicLen");
	

	

	// get the screen resolution in the CLD file which is loaded in window.d
	// and the current screen resolution and scale the controls in the cld file
	// so that the cld file will be displayed with the same look
	
	if(gGetScaleFlg(parent)) {    // do the scaling if the scaling flag is set
		double controlScaleFactor = gGetControlScale(parent);
		if(controlScaleFactor>0) {
			double dCxScale, dCyScale;
			RECT   MFMarginRect;

			gGetMFCxCyScale(parent, &dCxScale, &dCyScale);
			gGetMFMargins(parent, &MFMarginRect);

			// to reduce the accumulated error, a special trick is used 
			v.xPos  = (v.xPos+MFMarginRect.left)*controlScaleFactor*dCxScale + 0.5;
			v.yPos  = (v.yPos+MFMarginRect.top)*controlScaleFactor*dCyScale + 0.5;			 

			v.width  = v.width*controlScaleFactor*dCxScale  + 0.5;
			v.height = v.height*controlScaleFactor*dCyScale + 0.5;

			if(v.width<1)
				v.width = 1;
			if(v.height<1)
				v.height = 1;
		}
	}

	gAddAppendOffsets(parent, &v.yPos, &v.xPos);
	ctl = gAddGenericControl(parent, (int) v.yPos, (int) v.xPos, (int) v.height, (int) v.width, v.name, NULL);

		
	//figure out what the name is and load it
	gSetStringValue(ctl,gName(curnode));
	
	//figure out what the properties are and load them
	node=gFirst(curnode);
	while (node){
		char *nam=gName(node);
		char buf[128];
		gPopulateStringFromNode(curnode,buf,nam);
		
		if (strcmp(nam,"name") &&
			strcmp(nam,"x") &&
			strcmp(nam,"y") &&
			strcmp(nam,"width") &&
			strcmp(nam,"height") &&
			strcmp(nam,"hidden") &&
			strcmp(nam,"disabled") &&
			strcmp(nam,"helpTopicLen") &&
			strcmp(nam,"helpTopic"))
			gAddStr(ctl,nam,gNewWithStr(String,buf));
		
		node=gNext(node);
	}

	gPopulateStringFromNode(curnode,buf,"xpath");
	gSetXPathBinding(ctl,buf);
	
	// image file name
	if (v.imageFileNameLen) {
		p = v.imageFileNameLen > BUFLEN ? malloc((unsigned)v.imageFileNameLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"fileName");
		gSetStringValue(ctl, p);
		if (v.imageFileNameLen > BUFLEN)
			free(p);
	}

	// helpTopic
	if (v.helpTopicLen) {
		p = v.helpTopicLen > BUFLEN ? malloc((unsigned)v.helpTopicLen) : buf;
		if (!p)
			vError(Application, "out of memory");
		gPopulateStringFromNode(curnode,p,"helpTopic");
		gSetTopic(ctl, p);
		if (v.helpTopicLen > BUFLEN)
			free(p);
	}

	gSetControlParameters(ctl, &v);
	if (gModifyChildren(parent))
		gShow(ctl);
	return ctl;
}


imeth	gAddStr, <vAdd> (char *key, value)
{
	char *x=gStringValue(value);
	return gAddStr(iProperties,key,gNewWithStr(String,x));
}

imeth	gRemoveStr, <vRemove> (char *key)
{
	return gRemoveStr(iProperties,key);
}

imeth	gSequence ()
{
	return gSequence(iProperties);
}

imeth	char	*gStringValue()
{
	return (iTypeName)?gStringValue(iTypeName):"";
}

imeth	gSetStringValue(char *cp)
{
	if (iTypeName)
		gChangeStrValue(iTypeName, cp);
	else
		iTypeName=gNewWithStr(String,cp);
		
	return self;
}




