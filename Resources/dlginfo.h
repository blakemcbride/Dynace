/*
 * Used to create a window of "DIALOG" class.
 */
#define DIALOGCLASS     0x8002

#define ORDID_RT_RESOURCE32     0x00    // Aligned res file dummy resource.
#define ORDID_RT_DIALOG         0x05    // Dialog resource type.
#define ORDID_RT_MENU			0x04	// Menu ressource type
#define ORDID_RT_VERSION		0x10	// Version ressource type
#define ORDID_RT_DLGINCLUDE     0x11    // Dialog include file resource type.
#define ORDID_RT_ICON			0x3		// Icon resource
/*
 * The ordinal for the name of the DLGINCLUDE resource.
 */
#define ORDID_DLGINCLUDE_NAME   1


typedef struct {
	DWORD DataSize;                 // Size of data.
	DWORD HeaderSize;               // Size of the resource header.
} RES, *PRES;

typedef struct {
	DWORD DataVersion;              // Predefined resource data version.
	WORD MemoryFlags;               // Resource memory flags.
	WORD LanguageId;                // UNICODE support for NLS.
	DWORD Version;                  // Version of the resource data.
	DWORD Characteristics;          // Characteristics of the data.
} RES2, *PRES2;

typedef struct {
	DWORD Style;
	unsigned char Children;
	unsigned short x;
	unsigned short y;
	unsigned short cx;
	unsigned short cy;
	} DLGHEADER;

typedef struct {
    DWORD lStyle;                   // Style for the dialog.
    DWORD lExtendedStyle;           // The extended style.
    WORD NumberOfItems;             // Number of controls.
    WORD x;                         // Starting x location.
    WORD y;                         // Starting y location.
    WORD cx;                        // Dialog width.
    WORD cy;                        // Dialog height.
} DIALOGBOXHEADER,*PDIALOGBOXHEADER;

typedef struct tagControlItem {
    DWORD Style;                   // Style for the control.
    DWORD lExtendedStyle;           // The extended style.
    WORD x;                         // Starting x location.
    WORD y;                         // Starting y location.
    WORD cx;                        // Control width.
    WORD cy;                        // Control height.
    WORD ID;                       // Control id.
	char Class;
} CONTROLITEM;


#define SIZEOF_CONTROLITEM 18

typedef struct tagCLASSTEMPLATE {
	DWORD Scheme;
	WORD Atom;
} CLASSTEMPLATE;
#define RCH_ORDINAL_SCHEME (-1)

#define SIZEOF_DIALOGBOXHEADER  (                               \
    sizeof(DWORD) +                 /* lStyle           */      \
    sizeof(DWORD) +                 /* lExtendedStyle   */      \
    sizeof(WORD) +                  /* NumberOfItems    */      \
    sizeof(WORD) +                  /* x                */      \
    sizeof(WORD) +                  /* y                */      \
    sizeof(WORD) +                  /* cx               */      \
    sizeof(WORD)                    /* cy               */      \
    )

#define OrdID(psz)      (((PORDINAL)(psz))->wOrdID)


/*
 * The aligned ordinal structure.  Ordinals start with a word that is
 * always 0xffff, followed by a word that contains the ordinal id.
 */
typedef struct {
	WORD wReserved;
	WORD wOrdID;
} ORDINAL, *PORDINAL;

typedef struct _infdialog {
	int cx;
	int cy;
	int size;
	int Id;
	char *Name;
} GlobalDlgInfo;

extern GlobalDlgInfo thisDialog;
typedef void (*WorkFn)(char *,int,HWND);
void DumpResourceFile(char *start,int size,HWND hwnd);
int DumpResFile(char *filename,HWND h,WorkFn fn);
void BuildDir(HWND hwnd);
void DoBuildDir(char *start,int size,HWND hwnd);
HANDLE AddTreeItem(HWND hwndTV,LPSTR lpszItem,int level,int type,void *data,int rt_id);
HWND  ResourceDialogParam(
    char * ResFileName, // File name of the .res file
    LPCSTR lpName,      // pointer to resource name.Can be MAKEINTRESOURCE(int)
    HWND   parent,   // parent to dialog
    DLGPROC pDialogProc,  //  dialog procedure
    LPARAM  param         //  parameter
   );
BOOL CALLBACK DefDlg(HWND hDlg,UINT message,WPARAM wParam,LPARAM lParam);
HMENU ResourceLoadMenu(char *ResFileName,char *id);
HICON ResourceLoadIcon(char *ResFileName,char *id);
void DrawDIB(HDC hDC,LPSTR,int,int);
char *ResourceLoadBitmap(char *ResFileName,char *id);


   
