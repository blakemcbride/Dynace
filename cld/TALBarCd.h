

enum Symbology 
{
	Code39Normal,  Code39Full,  UPC_A,    UPC_E, 
	EAN8,          EAN13,       BookLan,  Interleaved2of5, 
	Discrete2of5,  CodaBar,     Code93,   Code128, 
	EAN_UCC128,    PDF417,      PostNet,  symbologyLAST
};

enum Orient {horizontal, vertical90, upsidedown, vertical270};

enum OutputOption {Clipboard,SaveToFile, MetafilePict, OutputTohDC, NoOutput};


typedef struct tagTALBarCode
{
	int       messageLength; 
	char      messageBuffer[100];
	int       commentLength;
	char      commentBuffer[100];
	int       narrowBarWidth;      // in mils
	int       barWidthReduction;   // percent of narrowBarWidth
	int       barCodeHeight;       // in mils
	COLORREF  fgColor;             // Foreground
	COLORREF  bgColor;             // Background
	int       narrowToWideRatio;   // 20-30  (=2.0 to 3.0)
	char      fontName[32];        // font name for human readable
	int       fontSize;            // font size in points
	COLORREF  textColor;
	enum      Orient orientation;
	long      preferences;
	int       horizontalDPI;
	int       verticleDPI;
	enum      OutputOption outputOption;

#ifdef  WIN32
	char      outputFilename[260];
#else
	char      outputFilename[68];
#endif

	HDC       outputhDC;           // Output device context (when outputting to hDC)
	float     XPosInInches;        // X page position (when outputting to hDC)
	float     YPosInInches;        // Y page position (when outputting to hDC)
	long      reserved;            // reserved for possible future 32 bit use
} TALBarCode;

#ifdef	WIN32
typedef int  (WINAPI *TALpFnct)(TALBarCode*, METAFILEPICT*);
#else
typedef int  (FAR PASCAL *TALpFnct)(TALBarCode*, METAFILEPICT*);
#endif

