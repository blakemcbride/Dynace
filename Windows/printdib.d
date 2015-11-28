

#include "generics.h"
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

static	HPALETTE hpalCurrent;        /* Handle to current palette            */
static	HANDLE   hdibCurrent;        /* Handle to current memory DIB         */
static	HBITMAP  hbmCurrent;         /* Handle to current memory BITMAP      */
static	HANDLE   hbiCurrent;         /* Handle to current bitmap info struct */
static	BOOL     fPalColors;         /* TRUE if the current DIB's color table   */
                                     /* contains palette indexes not rgb values */
static	BOOL     bLegitDraw; 	     /* We have a valid bitmap to draw       */
static	RECT     rcClip;
static	DWORD    dwOffset;
static	BOOL     bDIBToDevice; 	     /* Use SetDIBitsToDevice() to BLT data.         */

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : FreeDib(void)                                              *
 *                                                                          *
 *  PURPOSE    : Frees all currently active bitmap, DIB and palette objects *
 *               and initializes their handles.                             *
 *                                                                          *
 ****************************************************************************/
static	void	FreeDib()
{
	if (hpalCurrent)
		DeleteObject(hpalCurrent);

	if (hbmCurrent)
		DeleteObject(hbmCurrent);

	if (hdibCurrent)
		GlobalFree(hdibCurrent);

	if (hbiCurrent && hbiCurrent != hdibCurrent)
		GlobalFree(hbiCurrent);

	fPalColors  = FALSE;
	bLegitDraw  = FALSE;
	hpalCurrent = 0;
	hdibCurrent = 0;
	hbmCurrent  = 0;
	hbiCurrent  = 0;
	SetRectEmpty (&rcClip);
}


/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibNumColors(VOID FAR * pv)                                *
 *                                                                          *
 *  PURPOSE    : Determines the number of colors in the DIB by looking at   *
 *               the BitCount filed in the info block.                      *
 *                                                                          *
 *  RETURNS    : The number of colors in the DIB.                           *
 *                                                                          *
 ****************************************************************************/
static	WORD DibNumColors (VOID FAR * pv)
{
	INT                 bits;
	LPBITMAPINFOHEADER  lpbi;
	LPBITMAPCOREHEADER  lpbc;

	lpbi = ((LPBITMAPINFOHEADER)pv);
	lpbc = ((LPBITMAPCOREHEADER)pv);

	/*  With the BITMAPINFO format headers, the size of the palette
	 *  is in biClrUsed, whereas in the BITMAPCORE - style headers, it
	 *  is dependent on the bits per pixel ( = 2 raised to the power of
	 *  bits/pixel).
	 */
	if (lpbi->biSize != sizeof(BITMAPCOREHEADER)){
		if (lpbi->biClrUsed != 0)
			return (WORD)lpbi->biClrUsed;
		bits = lpbi->biBitCount;
	}
	else
		bits = lpbc->bcBitCount;

	switch (bits){
        case 1:
                return 2;
        case 4:
                return 16;
        case 8:
                return 256;
        default:
                /* A 24 bitcount DIB has no color table */
                return 0;
	}
}
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : lread(int fh, VOID FAR *pv, DWORD ul)                      *
 *                                                                          *
 *  PURPOSE    : Reads data in steps of 32k till all the data has been read.*
 *                                                                          *
 *  RETURNS    : 0 - If read did not proceed correctly.                     *
 *               number of bytes read otherwise.                            *
 *                                                                          *
 ****************************************************************************/
static	DWORD lread (
    INT       fh,
    VOID FAR      *pv,
    DWORD             ul)
{
	DWORD     ulT = ul;
	BYTE *hp = pv;

	while (ul > (DWORD)MAXREAD) {
		if (_lread(fh, (LPSTR)hp, (UINT)MAXREAD) != MAXREAD)
			return 0;
		ul -= MAXREAD;
		hp += MAXREAD;
	}
	if (_lread(fh, (LPSTR)hp, (UINT)ul) != (UINT)ul)
		return 0;
	return ulT;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : ReadBitMapFileHeaderandConvertToDwordAlign(HFILE fh, LPBITMAPFILEHEADER pbf)
 *                                                                          *
 *  PURPOSE    : read file header (which is packed) and convert into unpacked BITMAPFILEHEADER strucutre
 *                                                                          *
 *  RETURNS    : VOID
 *                                                                          *
 ****************************************************************************/

static VOID ReadBitMapFileHeaderandConvertToDwordAlign(HFILE fh, LPBITMAPFILEHEADER pbf, LPDWORD lpdwoff)
{
        DWORD off;

        off = _llseek(fh, 0L, (UINT) SEEK_CUR);
        *lpdwoff = off;

/*              BITMAPFILEHEADER STRUCUTURE is as follows 
 *              BITMAPFILEHEADER
 *              WORD    bfType 
 >          ....                  <     add WORD if packed here!
 *              DWORD   bfSize 
 *              WORD    bfReserved1
 *              WORD    bfReserved2
 *              DWORD   bfOffBits 
 *                      This is the packed format, unpacked adds a WORD after bfType
 */

        /* read in bfType*/
        _lread(fh, (LPSTR) &pbf->bfType, sizeof(WORD));
        /* read in last 3 dwords*/
        _lread(fh, (LPSTR) &pbf->bfSize, sizeof(DWORD) * 3);

}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : ReadDibBitmapInfo(int fh)                                  *
 *                                                                          *
 *  PURPOSE    : Will read a file in DIB format and return a global HANDLE  *
 *               to it's BITMAPINFO.  This function will work with both     *
 *               "old" (BITMAPCOREHEADER) and "new" (BITMAPINFOHEADER)      *
 *               bitmap formats, but will always return a "new" BITMAPINFO  *
 *                                                                          *
 *  RETURNS    : A handle to the BITMAPINFO of the DIB in the file.         *
 *                                                                          *
 ****************************************************************************/
static	HANDLE ReadDibBitmapInfo(INT fh)
{
	DWORD     off;
	HANDLE    hbi = 0;
	INT       size;
	INT       i;
	WORD      nNumColors;

	RGBQUAD FAR       *pRgb;
	BITMAPINFOHEADER   bi;
	BITMAPCOREHEADER   bc;
	LPBITMAPINFOHEADER lpbi;
	BITMAPFILEHEADER   bf;
	DWORD              dwWidth = 0;
	DWORD              dwHeight = 0;
	WORD               wPlanes, wBitCount;

	if (fh == -1)
		return 0;
#ifdef FIXDWORDALIGNMENT
	/* Reset file pointer and read file header */
	off = _llseek(fh, 0L, (UINT)SEEK_CUR);
	if ((SIZEOF_BITMAPFILEHEADER_PACKED)  != _lread(fh, (LPSTR)&bf, (UINT)sizeof (SIZEOF_BITMAPFILEHEADER_PACKED)))
		return FALSE;
#else
        ReadBitMapFileHeaderandConvertToDwordAlign(fh, &bf, &off);
        /* at this point we have read the file into bf*/
#endif

	/* Do we have a RC HEADER? */
	if (!ISDIB (bf.bfType)) {    
		bf.bfOffBits = 0L;               
                _llseek(fh, off, (UINT)SEEK_SET); /*seek back to beginning of file*/
	}
	if (sizeof (bi) != _lread(fh, (LPSTR)&bi, (UINT)sizeof(bi)))
		return FALSE;

	nNumColors = DibNumColors (&bi);

	/* Check the nature (BITMAPINFO or BITMAPCORE) of the info. block
	 * and extract the field information accordingly. If a BITMAPCOREHEADER,
	 * transfer it's field information to a BITMAPINFOHEADER-style block
	 */
	switch (size = (INT)bi.biSize){
        case sizeof (BITMAPINFOHEADER):
		break;

        case sizeof (BITMAPCOREHEADER):

		bc = *(BITMAPCOREHEADER*)&bi;

		dwWidth   = (DWORD)bc.bcWidth;
		dwHeight  = (DWORD)bc.bcHeight;
		wPlanes   = bc.bcPlanes;
		wBitCount = bc.bcBitCount;

		bi.biSize           = sizeof(BITMAPINFOHEADER);
		bi.biWidth              = dwWidth;
		bi.biHeight             = dwHeight;
		bi.biPlanes             = wPlanes;
		bi.biBitCount           = wBitCount;

		bi.biCompression        = BI_RGB;
		bi.biSizeImage          = 0;
		bi.biXPelsPerMeter      = 0;
		bi.biYPelsPerMeter      = 0;
		bi.biClrUsed            = nNumColors;
		bi.biClrImportant       = nNumColors;

		_llseek(fh, (LONG)sizeof(BITMAPCOREHEADER) - (LONG)sizeof(BITMAPINFOHEADER), (UINT)SEEK_CUR);
		break;

        default:
		/* Not a DIB! */
		return 0;
	}

	/*  Fill in some default values if they are zero */
	if (bi.biSizeImage == 0){
		bi.biSizeImage = WIDTHBYTES ((DWORD)bi.biWidth * bi.biBitCount)
			* bi.biHeight;
	}
	if (bi.biClrUsed == 0)
		bi.biClrUsed = DibNumColors(&bi);

	/* Allocate for the BITMAPINFO structure and the color table. */
	hbi = GlobalAlloc (GHND, (LONG)bi.biSize + nNumColors * sizeof(RGBQUAD));
	if (!hbi)
		return 0;
	lpbi = (VOID FAR *)GlobalLock (hbi);
	*lpbi = bi;

	/* Get a pointer to the color table */
	pRgb = (RGBQUAD FAR *)((LPSTR)lpbi + bi.biSize);
	if (nNumColors){
		if (size == sizeof(BITMAPCOREHEADER)){
			/* Convert a old color table (3 byte RGBTRIPLEs) to a new
			 * color table (4 byte RGBQUADs)
			 */
			_lread(fh, (LPSTR)pRgb, (UINT)nNumColors * sizeof(RGBTRIPLE));

			for (i = nNumColors - 1; i >= 0; i--){
				RGBQUAD rgb;

				rgb.rgbRed      = ((RGBTRIPLE FAR *)pRgb)[i].rgbtRed;
				rgb.rgbBlue     = ((RGBTRIPLE FAR *)pRgb)[i].rgbtBlue;
				rgb.rgbGreen    = ((RGBTRIPLE FAR *)pRgb)[i].rgbtGreen;
				rgb.rgbReserved = (BYTE)0;

				pRgb[i] = rgb;
			}
		}
		else
			_lread(fh, (LPSTR)pRgb, (UINT)nNumColors * sizeof(RGBQUAD));
	}

	if (bf.bfOffBits != 0L){
		_llseek(fh, off + bf.bfOffBits, (UINT)SEEK_SET);
        }
	GlobalUnlock(hbi);
	return hbi;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : CreateBIPalette(LPBITMAPINFOHEADER lpbi)                   *
 *                                                                          *
 *  PURPOSE    : Given a Pointer to a BITMAPINFO struct will create a       *
 *               a GDI palette object from the color table.                 *
 *                                                                          *
 *  RETURNS    : A handle to the palette.                                   *
 *                                                                          *
 ****************************************************************************/
static	HPALETTE	CreateBIPalette(LPBITMAPINFOHEADER lpbi)
{
	LOGPALETTE          *pPal;
	HPALETTE            hpal = 0;
	WORD                nNumColors;
	BYTE                red;
	BYTE                green;
	BYTE                blue;
	WORD                i;
	RGBQUAD        FAR *pRgb;

	if (!lpbi)
		return 0;

	if (lpbi->biSize != sizeof(BITMAPINFOHEADER))
		return 0;

	/* Get a pointer to the color table and the number of colors in it */
	pRgb = (RGBQUAD FAR *)((LPSTR)lpbi + (WORD)lpbi->biSize);
	nNumColors = DibNumColors(lpbi);

	if (nNumColors){
		/* Allocate for the logical palette structure */
/*		pPal = (LOGPALETTE*)LocalAlloc(LPTR,sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY)); */
		pPal = (LOGPALETTE *) calloc(1, sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));
		if (!pPal)
			return 0;

		pPal->palNumEntries = nNumColors;
		pPal->palVersion    = PALVERSION;

		/* Fill in the palette entries from the DIB color table and
		 * create a logical color palette.
		 */
		for (i = 0; i < nNumColors; i++){
			pPal->palPalEntry[i].peRed   = pRgb[i].rgbRed;
			pPal->palPalEntry[i].peGreen = pRgb[i].rgbGreen;
			pPal->palPalEntry[i].peBlue  = pRgb[i].rgbBlue;
			pPal->palPalEntry[i].peFlags = (BYTE)0;
		}
		hpal = CreatePalette(pPal);
		free(pPal);
/*		LocalFree((HANDLE)pPal);  */
	}
	else if (lpbi->biBitCount == 24){
		/* A 24 bitcount DIB has no color table entries so, set the number of
		 * to the maximum value (256).
		 */
		nNumColors = MAXPALETTE;
/*		pPal = (LOGPALETTE*)LocalAlloc(LPTR,sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));  */
		pPal = (LOGPALETTE *) calloc(1, sizeof(LOGPALETTE) + nNumColors * sizeof(PALETTEENTRY));
		if (!pPal)
			return 0;

		pPal->palNumEntries = nNumColors;
		pPal->palVersion    = PALVERSION;

		red = green = blue = 0;

		/* Generate 256 (= 8*8*4) RGB combinations to fill the palette
		 * entries.
		 */
		for (i = 0; i < pPal->palNumEntries; i++){
			pPal->palPalEntry[i].peRed   = red;
			pPal->palPalEntry[i].peGreen = green;
			pPal->palPalEntry[i].peBlue  = blue;
			pPal->palPalEntry[i].peFlags = (BYTE)0;

			if (!(red += 32))
				if (!(green += 32))
					blue += 64;
		}
		hpal = CreatePalette(pPal);
		free(pPal);
/*		LocalFree((HANDLE)pPal);  */
	}
	return hpal;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : CreateDibPalette(HANDLE hbi)                               *
 *                                                                          *
 *  PURPOSE    : Given a Global HANDLE to a BITMAPINFO Struct               *
 *               will create a GDI palette object from the color table.     *
 *               (BITMAPINFOHEADER format DIBs only)                                     *
 *                                                                          *
 *  RETURNS    : A handle to the palette.                                   *
 *                                                                          *
 ****************************************************************************/
static	HPALETTE	CreateDibPalette(HANDLE hbi)
{
	HPALETTE hpal;

	if (!hbi)
		return 0;
	hpal = CreateBIPalette((LPBITMAPINFOHEADER)GlobalLock(hbi));
	GlobalUnlock(hbi);
	return hpal;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : DibInfo(HANDLE hbi,LPBITMAPINFOHEADER lpbi)                *
 *                                                                          *
 *  PURPOSE    : Retrieves the DIB info associated with a CF_DIB            *
 *               format memory block.                                       *
 *                                                                          *
 *  RETURNS    : TRUE  - if successful.                                     *
 *               FALSE - otherwise                                          *
 *                                                                          *
 ****************************************************************************/
static	BOOL DibInfo (
    HANDLE hbi,
    LPBITMAPINFOHEADER lpbi)
{
	if (hbi){
		*lpbi = *(LPBITMAPINFOHEADER)GlobalLock (hbi);

		/* fill in the default fields */
		if (lpbi->biSize != sizeof (BITMAPCOREHEADER)){
			if (lpbi->biSizeImage == 0L)
                                lpbi->biSizeImage = WIDTHBYTES(lpbi->biWidth*lpbi->biBitCount) * lpbi->biHeight;

			if (lpbi->biClrUsed == 0L)
                                lpbi->biClrUsed = DibNumColors (lpbi);
		}
		GlobalUnlock (hbi);
		return TRUE;
	}
	return FALSE;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  PaletteSize(VOID FAR * pv)                                *
 *                                                                          *
 *  PURPOSE    :  Calculates the palette size in bytes. If the info. block  *
 *                is of the BITMAPCOREHEADER type, the number of colors is  *
 *                multiplied by 3 to give the palette size, otherwise the   *
 *                number of colors is multiplied by 4.                                                          *
 *                                                                          *
 *  RETURNS    :  Palette size in number of bytes.                          *
 *                                                                          *
 ****************************************************************************/
static	WORD PaletteSize(VOID FAR * pv)
{
	LPBITMAPINFOHEADER lpbi;
	WORD               NumColors;

	lpbi      = (LPBITMAPINFOHEADER)pv;
	NumColors = DibNumColors(lpbi);

	if (lpbi->biSize == sizeof(BITMAPCOREHEADER))
		return (WORD)(NumColors * sizeof(RGBTRIPLE));
	else
		return (WORD)(NumColors * sizeof(RGBQUAD));
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :OpenDIB(LPSTR szFile)                                       *
 *                                                                          *
 *  PURPOSE    :Open a DIB file and create a MEMORY DIB, a memory handle    *
 *              containing BITMAPINFO, palette data and the bits.           *
 *                                                                          *
 *  RETURNS    :A handle to the DIB.                                        *
 *                                                                          *
 ****************************************************************************/
static	HANDLE	OpenDIB(LPSTR szFile)
{
	HFILE               fh;
	BITMAPINFOHEADER    bi;
	LPBITMAPINFOHEADER  lpbi;
	DWORD               dwLen = 0;
	DWORD               dwBits;
	HANDLE              hdib;
	HANDLE              h;
	OFSTRUCT            of;

	/* Open the file and read the DIB information */
	fh = OpenFile(szFile, &of, (UINT)OF_READ);
	if (fh == -1) {
		gMoreHandles(LowFile);
		fh = OpenFile(szFile, &of, (UINT)OF_READ);
	}
	if (fh == -1)
		return 0;

	hdib = ReadDibBitmapInfo(fh);
	if (!hdib)
		return 0;
	DibInfo(hdib,&bi);

	/* Calculate the memory needed to hold the DIB */
	dwBits = bi.biSizeImage;
	dwLen  = bi.biSize + (DWORD)PaletteSize (&bi) + dwBits;

	/* Try to increase the size of the bitmap info. buffer to hold the DIB */
	h = GlobalReAlloc(hdib, dwLen, GHND);
	if (!h){
		GlobalFree(hdib);
		hdib = 0;
	}
	else
		hdib = h;

	/* Read in the bits */
	if (hdib){

		lpbi = (VOID FAR *)GlobalLock(hdib);
		lread(fh, (LPSTR)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi), dwBits);
		GlobalUnlock(hdib);
	}
	_lclose(fh);

	return hdib;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : BitmapFromDib(HANDLE hdib, HPALETTE hpal)                  *
 *                                                                          *
 *  PURPOSE    : Will create a DDB (Device Dependent Bitmap) given a global *
 *               handle to a memory block in CF_DIB format                  *
 *                                                                          *
 *  RETURNS    : A handle to the DDB.                                       *
 *                                                                          *
 ****************************************************************************/
static	HBITMAP	BitmapFromDib(HANDLE hdib, HPALETTE hpal)
{
	LPBITMAPINFOHEADER  lpbi;
	HPALETTE            hpalT;
	HDC                 hdc;
	HBITMAP             hbm;

	StartWait();

	if (!hdib)
		return 0;

	lpbi = (VOID FAR *)GlobalLock(hdib);

	if (!lpbi)
		return 0;

	hdc = GetDC(0);

	if (hpal){
		hpalT = SelectPalette(hdc,hpal,FALSE);
		RealizePalette(hdc);     // GDI Bug...????
	}

	hbm = CreateDIBitmap(hdc,
			     (LPBITMAPINFOHEADER)lpbi,
			     (LONG)CBM_INIT,
			     (LPSTR)lpbi + lpbi->biSize + PaletteSize(lpbi),
			     (LPBITMAPINFO)lpbi,
			     DIB_RGB_COLORS );

	if (hpal)
		SelectPalette(hdc,hpalT,FALSE);

	ReleaseDC(0,hdc);
	GlobalUnlock(hdib);

	EndWait();

	return hbm;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   : InitDIB(hWnd)                                              *
 *                                                                          *
 *  PURPOSE    : Reads a DIB from a file, obtains a handle to it's          *
 *               BITMAPINFO struct., sets up the palette and loads the DIB. *
 *                                                                          *
 *  RETURNS    : TRUE  - DIB loads ok                                       *
 *               FALSE - otherwise                                          *
 *                                                                          *
 ****************************************************************************/
static	int	InitDIB(char *achFileName)
{
	HFILE              fh;
	LPBITMAPINFOHEADER lpbi;
	WORD FAR *         pw;
	INT                i;
	BITMAPINFOHEADER   bi;
	OFSTRUCT           of;

	FreeDib();

	/* Open the file and get a handle to it's BITMAPINFO */

	fh = OpenFile(achFileName, (LPOFSTRUCT)&of, (UINT)OF_READ);
	if (fh == -1) {
		gMoreHandles(LowFile);
		fh = OpenFile(achFileName, (LPOFSTRUCT)&of, (UINT)OF_READ);
	}
	if (fh == -1) {
//		ErrMsg("Can't open file '%ls'", (LPSTR)achFileName);
		return FALSE;
	}
	hbiCurrent = ReadDibBitmapInfo(fh);

	dwOffset = _llseek(fh, 0L, (UINT)SEEK_CUR);
	_lclose(fh);

	if (hbiCurrent == 0) {
//		ErrMsg("%ls is not a Legitimate DIB File!", (LPSTR)achFileName);
		return FALSE;
	}
	DibInfo(hbiCurrent,&bi);

	/* Set up the palette */
	hpalCurrent = CreateDibPalette(hbiCurrent);
	if (hpalCurrent == 0) {
//		ErrMsg("CreatePalette() Failed");
		return FALSE;
	}

	/*  Convert the DIB color table to palette relative indexes, so
	 *  SetDIBits() and SetDIBitsToDevice() can avoid color matching.
	 *  We can do this because the palette we realize is identical
	 *  to the color table of the bitmap, ie the indexes match 1 to 1
	 *
	 *  Now that the DIB color table is palette indexes not RGB values
	 *  we must use DIB_PAL_COLORS as the wUsage parameter to SetDIBits()
	 */
	lpbi = (VOID FAR *)GlobalLock(hbiCurrent);
	if (lpbi->biBitCount != 24) {
		fPalColors = TRUE;

		pw = (WORD FAR *)((LPSTR)lpbi + lpbi->biSize);

		for (i=0; i<(INT)lpbi->biClrUsed; i++)
			*pw++ = (WORD)i;
	}
	GlobalUnlock(hbiCurrent);
	bLegitDraw = TRUE;

	/*  If the input bitmap is not in RGB FORMAT the banding code will
	 *  not work!  we need to load the DIB bits into memory.
	 *  if memory DIB, load it all NOW!  This will avoid calling the
	 *  banding code.
	 */
	hdibCurrent = OpenDIB(achFileName);

	/*  If the RLE could not be loaded all at once, exit gracefully NOW,
	 *  to avoid calling the banding code
	 */
	if ((bi.biCompression != BI_RGB) && !hdibCurrent){
//		ErrMsg ("Could not load RLE!");
		FreeDib();
		return FALSE;
	}

	if (hdibCurrent && !bDIBToDevice){
                hbmCurrent = BitmapFromDib(hdibCurrent,hpalCurrent);
                if (!hbmCurrent){
//			ErrMsg ("Could not create bitmap!");
			FreeDib();
			return FALSE;
                }
	}

	return TRUE;
}
/****************************************************************************
 *                                                                          *
 *  FUNCTION   : StretchDibBlt( HDC hdc,                                    *
 *                              int x, int y,                               *
 *                              int dx, int dy,                             *
 *                              HANDLE hdib,                                *
 *                              int x0, int y0,                             *
 *                              int dx0, int dy0,                           *
 *                              LONG rop)                                   *
 *                                                                          *
 *  PURPOSE    : Draws a bitmap in CF_DIB format, using StretchDIBits()     *
 *               taking the same parameters as StretchBlt().                *
 *                                                                          *
 *  RETURNS    : TRUE  - if function succeeds.                              *
 *               FALSE - otherwise.                                         *
 *                                                                          *
 ****************************************************************************/
static	BOOL StretchDibBlt (
    HDC hdc,
    INT x,
    INT y,
    INT dx,
    INT dy,
    HANDLE hdib,
    INT x0,
    INT y0,
    INT dx0,
    INT dy0,
    LONG rop)

{
	LPBITMAPINFOHEADER lpbi;
	LPSTR        pBuf;
	BOOL         f;

	if (!hdib)
		return PatBlt(hdc,x,y,dx,dy,rop);

	lpbi = (VOID FAR *)GlobalLock(hdib);

	if (!lpbi)
		return FALSE;

	pBuf = (LPSTR)lpbi + (WORD)lpbi->biSize + PaletteSize(lpbi);

	f = StretchDIBits ( hdc,
			    x, y,
			    dx, dy,
			    x0, y0,
			    dx0, dy0,
			    pBuf, (LPBITMAPINFO)lpbi,
			    DIB_RGB_COLORS,
			    rop);

	GlobalUnlock(hdib);
	return f;
}

/****************************************************************************
 *                                                                          *
 *  FUNCTION   :  PrintDIB(HDC hDC, int xPos, int yPos, int width, int height) *
 *                                                                          *
 *  PURPOSE    :  Set the DIB bits to the printer DC.                       *
 *                                                                          *
 ****************************************************************************/
static	VOID PrintDIB (
    HDC hDC,
    INT xPos,
    INT yPos,
    INT width,
    INT height)

{
	BITMAPINFOHEADER bi;
	INT dibX,  dibY;
	INT dibDX, dibDY;

	if (!bLegitDraw)
		return;

	DibInfo (hbiCurrent, &bi);

	if (IsRectEmpty (&rcClip)){
		dibX  = 0;
		dibY  = 0;
		dibDX = (INT)bi.biWidth;
		dibDY = (INT)bi.biHeight;
	}
	else{
		dibX  = rcClip.left;
		dibY  = (INT)bi.biHeight - 1 - rcClip.bottom;
		dibDX = rcClip.right  - rcClip.left;
		dibDY = rcClip.bottom - rcClip.top;
	}

	if (hdibCurrent){
		/* Stretch the DIB to printer DC */
		StretchDibBlt ( hDC,
				xPos,
				yPos,
				width,
				height,
				hdibCurrent,
				dibX,
				dibY,
				dibDX,
				dibDY,
				SRCCOPY);
	}
#if 0
	else if (achFileName[0]) {

		SetMapMode (hDC, MM_ANISOTROPIC);
		(VOID)SetViewportOrgEx (hDC, xPos, yPos, NULL);
		(VOID)SetViewportExtEx (hDC, width, height, NULL);

		BandDIB (hWnd, hDC, 0, 0);
	}
#endif
}

void	PrintBMP(HDC hDC, char *file, int yPos, int xPos, int height, int width)
{
	InitDIB(file);
	PrintDIB(hDC, xPos, yPos, width, height);
	FreeDib();
}


