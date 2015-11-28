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

#define	CHANDLESIZE			7

#define	X_SHIFT_FOR_PASTE	16
#define	Y_SHIFT_FOR_PASTE	16

#define	CTLTYPE_STATIC		1
#define	CTLTYPE_TEXT		2
#define	CTLTYPE_NUMERIC		3
#define	CTLTYPE_DATE		4
#define	CTLTYPE_PUSHBUTTON	5
#define	CTLTYPE_RADIOBUTTON	6
#define	CTLTYPE_CHECKBOX	7
#define	CTLTYPE_LISTBOX		8
#define	CTLTYPE_COMBOBOX	9
#define	CTLTYPE_RECT		10
#define	CTLTYPE_LINE		11
#define	CTLTYPE_IMAGE		12
#define	CTLTYPE_TIME		13

#ifndef MAX_LANGUAGES
#define	MAX_LANGUAGES		2
#define ENGLISH				0
#define	SPANISH				1
#endif


#define	CTLSAVEINT4	int	/*  4 byte integer  */



#if	defined(_WIN32)  &&  !defined(unix)
#include <pshpack2.h>
#endif


// Note: Do not change the order of the variables in the following structure
typedef	struct {
	short	version;
	short	cxInPixel;
	short	cyInPixel;
	double	metaFileScale;
	short	metaFileNameLen;
	short	logPixelsx;
	short	logPixelsy;
}	CLD_HEADER_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	short	len;
	char	right;

	char	filler;  // it is borrowed in cld design for saving
	                 // 1: not save to meta file; 0: save to meta file
	
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_STATIC_t;

typedef struct {
	char     name[46];

// size and position of the rectangle
	short    height;
	short    width;
	short    xPos;
	short    yPos;

// the status of the rectangle
	char     hidden;
	char     disabled;
	char     f3D;

// font used for the text inside the rectangle
	short    fontNameLen;
	short    fontSize;

// the text and its format inside the rectangle
	short    textLength;
	short    DT_Format; 
	char     textColor;

// frame of the rectangle
	short    frameThickness;
	char     frameColor;
	char     frameStyle;  

// pattern that fills the rectangle
	short    dotSize;     // not used
	short    stepSize;    // not used
	char     patternForeColor;
	char     patternBackColor;

	char     fill;

	short    helpTopicLen;

}	CTLTYPE_RECT_t;


typedef struct {
	char     name[46];

// size and position of the image control
	short    height;
	short    width;
	short    xPos;
	short    yPos;

// the status of the image control
	char     hidden;
	char     disabled;
	char     notSaveToMetaFile;

// the image file name length
	short    imageFileNameLen;

	short    helpTopicLen;

}	CTLTYPE_IMAGE_t;


typedef struct {
	char     name[46];

// size and position of the line
	short    height;
	short    width;
	short    xPos;
	short    yPos;

// the status of the line
	char     hidden;      // not used
	char     disabled;    // not used

// the attributes of the line
	char     lineColor;
	char     lineStyle;  

	short    helpTopicLen;

}	CTLTYPE_LINE_t;


typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	short	minLen;
	short	maxLen;
	short	capitalize;
	short	defaultLen;
	short	helpTopicLen;
	short	fontNameLen;
	short	fontSize;

	unsigned CTLSAVEINT4 textCtlStyle;
}	CTLTYPE_TEXT_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	double	minimum;
	double	maximum;
	short	dp;
	char	format[8];
	short	fldWidth;
	double	defaultVal;
	short	helpTopicLen;
	char	right;

	char	filler;
	
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_NUMERIC_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	CTLSAVEINT4	minimum;
	CTLSAVEINT4	maximum;
	CTLSAVEINT4	defaultVal;
	char	format[20];
	short	helpTopicLen;
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_DATE_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	CTLSAVEINT4	minimum;
	CTLSAVEINT4	maximum;
	CTLSAVEINT4	defaultVal;
	char	format[20];
	short	helpTopicLen;
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_TIME_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	short	len;
	short	helpTopicLen;
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_PUSHBUTTON_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	short	len;
	short	defaultVal;
	char	next[46];
	short	helpTopicLen;
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_RADIOBUTTON_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	short	len;
	short	defaultVal;
	short	onStrLen;
	short	offStrLen;
	short	helpTopicLen;
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_CHECKBOX_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	short	type;
	short	items;
	short	defaultVal;
	short	helpTopicLen;
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_LISTBOX_t;

typedef struct {
	char	name[46];
	short	height;
	short	width;
	short	xPos;
	short	yPos;
	char	hidden;
	char	disabled;
	short	type;
	short	items;
	short	defaultVal;
	short	helpTopicLen;
	short	fontNameLen;
	short	fontSize;
}	CTLTYPE_COMBOBOX_t;


#if	defined(_WIN32)  &&  !defined(unix)
#include <poppack.h>
#endif





