; Dialog Box Command IDs
(define IDOK     1)
(define IDCANCEL 2)
(define IDABORT  3)
(define IDRETRY  4)
(define IDIGNORE 5)
(define IDYES    6)
(define IDNO     7)
(define IDCLOSE  8)
(define IDHELP   9)

;  MessageBox() Flags
(define MB_OK               #x00000000)
(define MB_OKCANCEL         #x00000001)
(define MB_ABORTRETRYIGNORE #x00000002)
(define MB_YESNOCANCEL      #x00000003)
(define MB_YESNO            #x00000004)
(define MB_RETRYCANCEL      #x00000005)

(define MB_APPLMODAL        #x00000000)
(define MB_SYSTEMMODAL      #x00001000)
(define MB_TASKMODAL        #x00002000)
(define MB_HELP             #x00004000)

;  Scaling modes
(define SM_10_PER_SYSCHAR   #b0000)  ;  10 points per system character
(define SM_PIXELS           #b0001)  ;  straight pixels
(define SM_1_PER_CHAR       #b0010)  ;  1 per a particlar font

;  Print Dialog modes
(define PM_LETTER           #b0000)
(define PM_LEGAL            #b0001)
(define PM_PORTRAIT         #b0000)
(define PM_LANDSCAPE        #b0010)

;  Window Styles
(define WS_OVERLAPPED       #x00000000)
(define WS_POPUP            #x80000000)
(define WS_CHILD            #x40000000)
(define WS_MINIMIZE         #x20000000)
(define WS_VISIBLE          #x10000000)
(define WS_DISABLED         #x08000000)
(define WS_CLIPSIBLINGS     #x04000000)
(define WS_CLIPCHILDREN     #x02000000)
(define WS_MAXIMIZE         #x01000000)
(define WS_CAPTION          #x00C00000)
(define WS_BORDER           #x00800000)
(define WS_DLGFRAME         #x00400000)
(define WS_VSCROLL          #x00200000)
(define WS_HSCROLL          #x00100000)
(define WS_SYSMENU          #x00080000)
(define WS_THICKFRAME       #x00040000)
(define WS_GROUP            #x00020000)
(define WS_TABSTOP          #x00010000)

(define WS_MINIMIZEBOX      #x00020000)
(define WS_MAXIMIZEBOX      #x00010000)

;  Common Window Styles
(define WS_OVERLAPPEDWINDOW (bitwise-ior WS_OVERLAPPED WS_CAPTION WS_SYSMENU WS_THICKFRAME WS_MINIMIZEBOX WS_MAXIMIZEBOX))

(define WS_POPUPWINDOW (bitwise-ior WS_POPUP WS_BORDER WS_SYSMENU))

(define WS_CHILDWINDOW WS_CHILD)

(define WS_TILED            WS_OVERLAPPED)
(define WS_ICONIC           WS_MINIMIZE)
(define WS_SIZEBOX          WS_THICKFRAME)
(define WS_TILEDWINDOW      WS_OVERLAPPEDWINDOW)

; Listbox Styles
(define LBS_NOTIFY            #x0001)
(define LBS_MULTIPLESEL       #x0008)
(define LBS_NOINTEGRALHEIGHT  #x0100)
(define LBS_DISABLENOSCROLL   #x1000)

;  Window Position Constants
(define HWND_TOP       0)
(define HWND_BOTTOM    1)
(define HWND_TOPMOST   -1)
(define HWND_NOTOPMOST -2)

; Stock Logical Objects
(define WHITE_BRUSH         0)
(define LTGRAY_BRUSH        1)
(define GRAY_BRUSH          2)
(define DKGRAY_BRUSH        3)
(define BLACK_BRUSH         4)
(define NULL_BRUSH          5)
(define HOLLOW_BRUSH        NULL_BRUSH)
(define WHITE_PEN           6)
(define BLACK_PEN           7)
(define NULL_PEN            8)
(define OEM_FIXED_FONT      10)
(define ANSI_FIXED_FONT     11)
(define ANSI_VAR_FONT       12)
(define SYSTEM_FONT         13)
(define DEVICE_DEFAULT_FONT 14)
(define DEFAULT_PALETTE     15)
(define SYSTEM_FIXED_FONT   16)
(define DEFAULT_GUI_FONT    17)

; Brush Styles
(define BS_SOLID            0)
(define BS_NULL             1)
(define BS_HOLLOW           BS_NULL)
(define BS_HATCHED          2)
(define BS_PATTERN          3)
(define BS_INDEXED          4)
(define BS_DIBPATTERN       5)
(define BS_DIBPATTERNPT     6)
(define BS_PATTERN8X8       7)
(define BS_DIBPATTERN8X8    8)
(define BS_MONOPATTERN      9)

; Hatch Styles 
(define HS_HORIZONTAL       0)       ; ----- 
(define HS_VERTICAL         1)       ; ||||| 
(define HS_FDIAGONAL        2)       ; \\\\\ 
(define HS_BDIAGONAL        3)       ; ///// 
(define HS_CROSS            4)       ; +++++ 
(define HS_DIAGCROSS        5)       ; xxxxx 


(define S_IREAD		#o400)
(define S_IWRITE	#o200)
(define O_BINARY	#x8000)
(define O_TEXT		#x4000)

; Rectangle control styles
(define DT_TOP             #x00000000)
(define DT_LEFT            #x00000000)
(define DT_CENTER          #x00000001)
(define DT_RIGHT           #x00000002)
(define DT_VCENTER         #x00000004)
(define DT_BOTTOM          #x00000008)
(define DT_WORDBREAK       #x00000010)
(define DT_SINGLELINE      #x00000020)
(define DT_EXPANDTABS      #x00000040)
(define DT_TABSTOP         #x00000080)
(define DT_NOCLIP          #x00000100)
(define DT_EXTERNALLEADING #x00000200)
(define DT_CALCRECT        #x00000400)
(define DT_NOPREFIX        #x00000800)
(define DT_INTERNAL        #x00001000)
