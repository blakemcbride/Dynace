

#include "generics.h"
#undef	Class
#undef	Scheme
#include <windows.h>
#include <stdio.h>
#include "dlginfo.h"
#pragma pack(1)
char *NameOrdCpyW(LPTSTR pNameOrdDest,LPTSTR pNameOrdSrc);
static BYTE abResource32[] = {
    0x00, 0x00, 0x00, 0x00,                 // DataSize (0 bytes).
    0x20, 0x00, 0x00, 0x00,                 // HeaderSize (32 bytes).
    0xff, 0xff, 0x00, 0x00,                 // Type (RT_RESOURCE32).
    0xff, 0xff, 0x00, 0x00,                 // Name (ordinal 0).
    0x00, 0x00, 0x00, 0x00,                 // DataVersion
    0x00, 0x00,                             // MemoryFlags
    0x00, 0x00,                             // LanguageId
    0x00, 0x00, 0x00, 0x00,                 // Version
    0x00, 0x00, 0x00, 0x00                  // Characteristics
};

typedef struct IconDirectoryEntry {
    BYTE    bWidth;
    BYTE    bHeight;
    BYTE    bColorCount;
    BYTE    bReserved;
    WORD    wPlanes;
    WORD    wBitCount;
    DWORD   dwBytesInRes;
    DWORD   dwImageOffset;
} ICONDIRENTRY;
typedef struct ICONDIR {
    WORD    idReserved;
    WORD    idType;
    WORD    idCount;
} ICON_HEADER;

struct ResourceDirectory {
     BYTE   bWidth;
     BYTE   bHeight;
     BYTE   bColorCount;
     BYTE   bReserved;
     WORD   wPlanes;
     WORD   wBitCount;
     DWORD  lBytesInRes;
     WORD   wNameOrdinal;
};


static int IsOrd(char *psz)
{
    ORDINAL *pord;
    
	pord = (ORDINAL *) psz;
    if (pord->wReserved == 0xFFFF)
        return (TRUE);
    else
        return (FALSE);
}


static char *ResType(PRES *pRes)
{
    /*
     * Skip past the two size fields.
     */
    return (LPTSTR)((PBYTE)pRes + sizeof(DWORD) + sizeof(DWORD));
}
static DWORD ResourceSize(PRES pRes)
{
    return pRes->HeaderSize + pRes->DataSize;
}

static char *ResourceName(PRES pRes)
{
    PBYTE pb;
	unsigned short *ps;

    /*
     * Skip past the two size fields.
     */
    pb = (PBYTE)pRes + sizeof(DWORD) + sizeof(DWORD);

    /*
     * Skip past the "Type" field to the name.
     */
    if (IsOrd(pb))
        pb = (LPTSTR)((PBYTE)pb + sizeof(ORDINAL));
    else {
		int l = 0;
		ps = (unsigned short *)pb;
		while (*ps) {
			l++;
			ps++;
		}
		l++;
		ps++;
        pb = (PBYTE)ps;
	}
	return (LPTSTR)pb;
}
static char *NameOrdCpyW(LPTSTR pNameOrdDest,LPTSTR pNameOrdSrc)
{
	if (IsOrd(pNameOrdSrc)) {
		memcpy((char *) pNameOrdDest, (char *) pNameOrdSrc, sizeof(ORDINAL));
		return (char *) pNameOrdDest + sizeof(ORDINAL);
	}
	else {
		unsigned short *p = (unsigned short *)pNameOrdDest;
		mbstowcs((LPWSTR)pNameOrdDest,pNameOrdSrc,strlen(pNameOrdSrc));
        while (*pNameOrdSrc) {
			p++;
			pNameOrdSrc++;
		}
		*p++ = 0;
        return (PBYTE)p;
	}
}


static PBYTE SkipResHeader(PRES pRes)
{
    return (PBYTE)pRes + pRes->HeaderSize;
}

static char *ResourceType(PRES pRes)
{
    /*
     * Skip past the two size fields.
     */
    return (char *)((PBYTE)pRes + sizeof(DWORD) + sizeof(DWORD));
}

static void ConvertWideString(char *src,char *dst)
{
	unsigned short *p;

	p = (unsigned short *)src;
	while (*p) {
		*dst++ = (char)*p++;
	}
	*dst = 0;
}

static INT NameOrdLen(LPTSTR pNameOrd)
{
	if (IsOrd(pNameOrd))
		return sizeof(ORDINAL);
	else
		return (lstrlen(pNameOrd) + 1) * sizeof(wchar_t);
}
static INT NameOrdLenW(LPTSTR pNameOrd)
{
	if (IsOrd(pNameOrd))
		return sizeof(ORDINAL);
	else
		return (wcslen((LPWSTR)pNameOrd) + 1) * sizeof(wchar_t);
}

static char *SkipSz(LPTSTR pNameOrd)
{
	if (IsOrd(pNameOrd))
		pNameOrd = (LPTSTR) ((char *) pNameOrd + sizeof(ORDINAL));
	else
		pNameOrd += lstrlen(pNameOrd) + 1;
	return (char *) pNameOrd;
}

static VOID DWordAlign(char **ppb)
{
    *ppb += (4 - (((WORD)(DWORD)*ppb) & 3)) % 4;
}

static PRES2 ResourcePart2(PRES pRes)
{
    PBYTE pb;

    /*
     * Skip past the first part of the resource header.
     */
    pb = (PBYTE)pRes + sizeof(RES);

    /*
     * Skip past the "Type" field to the name.
     */
    pb = SkipSz((LPTSTR)pb);

    /*
     * Skip past the name field also.
     */
    pb = SkipSz((LPTSTR)pb);
    DWordAlign(&pb);

    return (PRES2)pb;
}

static CONTROLITEM *ParseDialogBoxHeader(PDIALOGBOXHEADER pdbh, int exStyle)
{
	CLASSTEMPLATE *ct;
	BYTE *pb;
	DWORD style,ExtStyle;
	char *MenuName,*Class,*Caption,*FontName,AsciiCaption[512];
	int PointSize;

	if (exStyle) {
		style = pdbh->lExtendedStyle;
		ExtStyle = pdbh->lStyle;
	}
	else {
		style = pdbh->lStyle;
		ExtStyle = pdbh->lExtendedStyle;
	}
	printf("Style 0x%x, Extended style 0x%x\nNumber of items %d. ",style,ExtStyle,pdbh->NumberOfItems);
	printf("Coords: x = %d, y = %d, cx = %d, cy = %d\n",pdbh->x,
			pdbh->y, pdbh->cx, pdbh->cy);
	pb = (char *) pdbh + SIZEOF_DIALOGBOXHEADER;
	MenuName = (LPTSTR) pb;
	pb += NameOrdLenW((LPTSTR) pb);
	Class = (LPTSTR) pb;
	ct = (CLASSTEMPLATE *)pb;
	if (ct->Scheme == RCH_ORDINAL_SCHEME || ct->Scheme == 0xFFFF0000) {
		pb += sizeof(CLASSTEMPLATE);
	}
	else {
		pb += NameOrdLenW((LPTSTR) pb);
	}
	Caption = (LPTSTR) pb;
	ConvertWideString(Caption,AsciiCaption);
	printf("Caption: \"%s\"\n",AsciiCaption);
	pb += (wcslen((LPWSTR) pb) + 1) * sizeof(WCHAR);
	/* Does the template specify a font? */
	if (style & DS_SETFONT) {
		PointSize = (SHORT) (*(WORD *) pb);
		pb += sizeof(WORD);
		if (*((unsigned short *)pb) == 0) pb += 4;
		FontName = (LPTSTR) pb;
		ConvertWideString(FontName,AsciiCaption);
		printf("Font \"%s\" %d\n",AsciiCaption,PointSize);
		pb += (wcslen((LPWSTR) pb) + 1) * sizeof(WCHAR);
	}
	else {
		PointSize = 0;
		FontName = NULL;
	}
	DWordAlign((PBYTE *)&pb);
	return (CONTROLITEM *) pb;
}

static int CheckExtendedStyle(PDIALOGBOXHEADER *ppdbh)
{
	PDIALOGBOXHEADER pdbh = *ppdbh;
	unsigned short *puShort = (unsigned short *)pdbh;
	if (puShort[1] == 0xFFFF) {
		pdbh = (PDIALOGBOXHEADER)((char *)pdbh + 8);
		*ppdbh = pdbh;
		return(1);
	}
	return(0);
}
static CONTROLITEM *ParseControlData(CONTROLITEM * pcd, int fextStyle)
{
	char *pb,*Class,*Text,Ascii[250];
	CLASSTEMPLATE *lpvdict;
	int Style,ExtStyle;

	if (fextStyle) {
		pcd = (CONTROLITEM *)((PBYTE)pcd + 4);
		Style = pcd->lExtendedStyle;
		ExtStyle = pcd->Style;
	}
	else {
		Style = pcd->Style;
		ExtStyle = pcd->lExtendedStyle;
	}
	printf("Control Id = %d\n",pcd->ID);
	printf("Style= 0x%x, extended style 0x%x. Coords: x=%d, y=%d, cx=%d, cy=%d\n",
		Style,ExtStyle,pcd->x,pcd->y,pcd->cx,pcd->cy);
    pb = (PBYTE)pcd + SIZEOF_CONTROLITEM;
    Class = (LPTSTR)pb;
	lpvdict = (CLASSTEMPLATE *)pb;
	if (lpvdict->Scheme == RCH_ORDINAL_SCHEME || lpvdict->Scheme == 0xFFFF0000) {
		pb += sizeof(CLASSTEMPLATE);
	}
	else {
		if (pb[0] == 0 && pb[1] == 0) pb += 2;
	    Class = (LPTSTR)pb;
		pb += NameOrdLenW((LPTSTR)pb);
	}
    Text = (LPTSTR)pb;
	ConvertWideString(Text,Ascii);
	printf("Text \"%s\"\n",Ascii);
	if (IsOrd(Class))
		printf("Class = 0x%x\n",OrdID(Class));
	else {
		printf("Class = 0x%d\n",*Class);
	}
    pb += NameOrdLenW((LPTSTR)pb);

    /*
     * Finally, skip the Create Struct Data.
     * After this, pb will be pointing to the next control.
     */
    pb += *(PWORD)pb + sizeof(WORD);

    DWordAlign((PBYTE *)&pb);

    return (CONTROLITEM *)pb;
}
static char *ReadNewItem(char *p,HMENU hnewmenu);

static char *ReadNewPopup(char *p,HMENU hnewmenu)
{
        unsigned short *pflags,*pText;
        char menutitle[256];
        int i,counter,Flags,last;
		HMENU hnew;

        pflags = (unsigned short *)p;
        Flags = *pflags;
        p += sizeof(unsigned short);
        i = 0;
        pText = (unsigned short *)p;
        i = wcstombs(menutitle,pText,255);
        p += (i+1) * sizeof(short);
		last = GetMenuItemCount(hnewmenu);
		hnew = CreateMenu();
        counter = 0;
        do {
                pflags = (unsigned short *)p;
                if (*pflags & MF_POPUP) {
                        p = ReadNewPopup(p,hnew);
                }
                else {
                        p = ReadNewItem(p,hnew);
                }
                counter++;
                if (counter > 255) {
                        MessageBox(0,"error","Menu with more than 255 items!",MB_OK);
                        break;
                }
                if (p == NULL) break;
        } while ((*pflags & MF_END) == 0);
		AppendMenu(hnewmenu,MF_POPUP|MF_STRING,(INT_PTR)hnew,menutitle);
        return(p);
}

static char *ReadNewItem(char *p,HMENU hnewmenu)
{
        unsigned short *pflags,*pText;
        int i,Flags,Id;
        char menuitemtitle[256];

        pflags = (unsigned short *)p;
        Flags = *pflags;
        p += sizeof(unsigned short);
        pflags = (unsigned short *)p;
        Id = *pflags;
        p += sizeof(unsigned short);
        pText = (unsigned short *)p;
        i = wcstombs(menuitemtitle,pText,255);
        p += (i+1) * sizeof(short);
		if (*menuitemtitle== 0 && Id == 0 && (Flags & MF_END) == 0)
			Flags |= MF_SEPARATOR;
		AppendMenu(hnewmenu,Flags & ~MF_END,Id,menuitemtitle);
        return(p);
}

static HMENU ReadMenu(char *res)
{
        char *p;
        unsigned short *pflags,flags;
		HMENU hnewmenu = CreateMenu();

        p = (char *)res;
        /* Skip header (not used since windows 3.1) */
        p += sizeof(unsigned short);
        p += sizeof(unsigned short);
        do {
                pflags = (unsigned short *)p;
                flags = *pflags;
                if (flags & MF_POPUP) {
                        p = ReadNewPopup(p,hnewmenu);
                }
                else
                        p = ReadNewItem(p,hnewmenu);
                if (p == NULL) break;
                if (flags & MF_END) {
                        break;
                }
        } while (1);
        return(hnewmenu);
}

static HWND 	SearchResource(LPCSTR lpName,
					   int type,
					 DLGPROC pDialogProc,
					 LPARAM param,
					 char *lpFileBase,
					 int TotalFileSize,
					 HWND parentWnd)
{
	char *pResAll = lpFileBase + TotalFileSize;
	PRES pRes = (PRES)lpFileBase;
	char pszSearchedName[256];
	char *pszType = ResourceType(pRes);
	char *pszName;
	int size = ResourceSize(pRes);	
	char Name[512],*bb;
	PDIALOGBOXHEADER pdbh;
	HWND result;
	UINT_PTR ul;

	ul = (UINT_PTR)lpName;
	if (HIWORD(ul) == 0) {
		sprintf(pszSearchedName,"%d",LOWORD(ul));
	}
	else
	if (IsOrd((char *)lpName)) {
		sprintf(pszSearchedName,"%d",OrdID(lpName));
	}
	else if (lpName[1] == 0 && lpName[3] == 0) {
		ConvertWideString((char *)lpName,(char *)pszSearchedName);
	}
	else {
		strcpy(pszSearchedName,lpName);
	}
	do {
		if (pRes->HeaderSize+pRes->DataSize == 0)
			break;
		if (IsOrd((char *)pRes) && OrdID(pRes) == ORDID_RT_DLGINCLUDE) {
			/* Ignore include files */
			;
		}
		else if (IsOrd((char *)pRes) &&
				OrdID(pRes) == ORDID_RT_RESOURCE32) {
	        /*
            * This is the dummy resource that identifies a
            * 32 bit resource file.  This resource should be
            * skipped also.
            */
			}
			else {
			/* This is some other kind of a resource. See if it matches */
				if (pRes->DataSize) {
					int size = ResourceSize(pRes);	
					pszType = ResourceType(pRes);
					if (IsOrd(pszType) && OrdID(pszType) == type) {	
					    pszName = ResourceName(pRes);
						if (!IsOrd(pszName)) {
							if (pszName[1] == 0 && pszName[3] == 0)
								ConvertWideString(pszName,Name);
							else
								strcpy(Name,pszName);
						}
						else {
							sprintf(Name,"%d",OrdID(pszName));
						}
						if (!strcmp(Name,pszSearchedName)) {
							/* found it.*/
							if (type == 5) {
								/* Build a dialog box */
								pdbh = (PDIALOGBOXHEADER) SkipResHeader(pRes);
								bb = malloc(size);
								memcpy(bb,pdbh,size);
								result = CreateDialogIndirectParam(GetModuleHandle(NULL),
									(LPDLGTEMPLATE)bb,
									parentWnd,
									pDialogProc,param);
								free(bb);
								return result;
							}
							else if (type == 4) {
								/* Build a menu */
								return (HWND) ReadMenu((char *)SkipResHeader(pRes));
							}
							else if (type == 3) {
								/* Build an Icon */
								HICON hicon;
								hicon = CreateIconFromResource((PBYTE)SkipResHeader(pRes),
									size,
									TRUE,
									0x30000);
								return (HWND)hicon;
							}
							else if (type == 1) {
								/* Build a cursor */
								return (HWND)CreateIconFromResource((PBYTE)SkipResHeader(pRes),
									size,
									FALSE,
									0x30000);
							}
							else if (type == 2 || type == 14) {
								/* Build bitmap */
								char *start = SkipResHeader(pRes);
								bb = malloc(size);
								memcpy(bb,start,size);
								return (HWND) bb;
							}
						}
					}

				}
			}
			/* Move to the next resource. */
		pRes = (PRES) (((char *) pRes) + pRes->HeaderSize + pRes->DataSize);
        DWordAlign((PBYTE *)&pRes);
	} while (pRes < (PRES) ((char *) pResAll + size));
	return (HWND) 0;
}

static HWND  ResourceDialogParamInternal(
    char * ResFileName, // File name of the .res file
	int typeOfResource,
    LPCSTR lpName,      // pointer to resource name.Can be MAKEINTRESOURCE(int)
    HWND   parent,   // parent to dialog
    DLGPROC pDialogProc,  //  dialog procedure
    LPARAM  param         //  parameter
   )
{
	void *lpFileBase;
	int TotalFileSize;
	HWND result = (HWND)0;
	FILE *inputFile;

	inputFile = fopen(ResFileName,"rb");
	if (!inputFile) {
		gMoreHandles(LowFile);
		inputFile = fopen(ResFileName,"rb");
	}
	if (inputFile == NULL)
		return (HWND)0;
	fseek(inputFile,0,SEEK_END);
	TotalFileSize = ftell(inputFile);
	fseek(inputFile,0,SEEK_SET);
	lpFileBase = malloc(TotalFileSize);
	if (lpFileBase == NULL) {
		fclose(inputFile);
		return (HWND)0;
	}
	fread(lpFileBase,1,TotalFileSize,inputFile);
	fclose(inputFile);

	if (!memcmp(lpFileBase,abResource32,32)) {
		result = SearchResource(lpName,typeOfResource,pDialogProc,param,lpFileBase,TotalFileSize,parent);
	}
	free(lpFileBase);
	return result;

}
HWND  ResourceDialogParam(
    char * ResFileName, // File name of the .res file
    LPCSTR lpName,      // pointer to resource name.Can be MAKEINTRESOURCE(int)
    HWND   parent,   // parent to dialog
    DLGPROC pDialogProc,  //  dialog procedure
    LPARAM  param         //  parameter
   )
{
	return ResourceDialogParamInternal(ResFileName,5,lpName,parent,pDialogProc,param);
}
HMENU ResourceLoadMenu(char *ResFileName,char *id)
{
	return (HMENU)ResourceDialogParamInternal(ResFileName,4,id,NULL,NULL,0);
}
HICON ResourceLoadIcon(char *ResFileName,char *id)
{
	char *res = (char *)ResourceDialogParamInternal(ResFileName,14,id,NULL,NULL,0);
	ICON_HEADER *dir = (ICON_HEADER  *)res;
	struct ResourceDirectory *dirEntry;
	char *save = res;
	int i;
	HICON result = 0;

	res += 6; // add up the icon header size
	for (i=0; i<dir->idCount;i++) {
		dirEntry = (struct ResourceDirectory *)res;
		result = (HICON)ResourceDialogParamInternal(ResFileName,3,
			MAKEINTRESOURCE(dirEntry->wNameOrdinal),NULL,NULL,0);
		if (result) break;
		res += sizeof(struct ResourceDirectory);
	}
	free(save);
	return result;
}
HCURSOR ResourceLoadCursor(char *ResFileName,char *id)
{
	return (HICON)ResourceDialogParamInternal(ResFileName,1,id,NULL,NULL,0);
}
char *ResourceLoadBitmap(char *ResFileName,char *id)
{
	return (char *)ResourceDialogParamInternal(ResFileName,2,id,NULL,NULL,0);
}
