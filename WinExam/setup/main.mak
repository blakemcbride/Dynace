# Microsoft Visual C++ generated build script - Do not modify

PROJ = MAIN
DEBUG = 1
PROGTYPE = 0
CALLER = 
ARGS = 
DLLS = 
D_RCDEFINES = /d_DEBUG 
R_RCDEFINES = /dNDEBUG 
ORIGIN = MSVC
ORIGIN_VER = 1.00
PROJPATH = .\
USEMFC = 0
CC = cl
CPP = cl
CXX = cl
CCREATEPCHFLAG = 
CPPCREATEPCHFLAG = 
CUSEPCHFLAG = 
CPPUSEPCHFLAG = 
FIRSTC = MAIN.C      
FIRSTCPP =             
RC = rc
CFLAGS_D_WEXE = /nologo /Gs /G2 /W3 /Gf /Zi /AL /Or /D "WINVER"="0x0310" /D "_DEBUG" /FR /GA /GEf /Fd"MAIN.PDB"
CFLAGS_R_WEXE = /nologo /Gs /G2 /W3 /Gf /AL /Oi /Ot /D "WINVER"="0x0310" /D "NDEBUG" /FR /GA /GEf 
LFLAGS_D_WEXE = /NOLOGO /NOD /NOE /NOI /PACKC:61440 /SEG:256 /ONERROR:NOEXE /CO /batch
LFLAGS_R_WEXE = /NOLOGO /NOD /NOE /NOI /PACKC:61440 /SEG:256 /ONERROR:NOEXE  
LIBS_D_WEXE = dwdswm dynldm oldnames llibcew libw oldnames ole2 commdlg.lib ddeml.lib 
LIBS_R_WEXE = dwdswm dynldm oldnames llibcew libw oldnames ole2 commdlg.lib ddeml.lib 
RCFLAGS = /nologo 
RESFLAGS = /nologo /k
RUNFLAGS = 
DEFFILE = MAIN.DEF
OBJS_EXT = 
LIBS_EXT = 
!if "$(DEBUG)" == "1"
CFLAGS = $(CFLAGS_D_WEXE)
LFLAGS = $(LFLAGS_D_WEXE)
LIBS = $(LIBS_D_WEXE)
MAPFILE = nul
RCDEFINES = $(D_RCDEFINES)
!else
CFLAGS = $(CFLAGS_R_WEXE)
LFLAGS = $(LFLAGS_R_WEXE)
LIBS = $(LIBS_R_WEXE)
MAPFILE = nul
RCDEFINES = $(R_RCDEFINES)
!endif
!if [if exist MSVC.BND del MSVC.BND]
!endif
SBRS = MAIN.SBR


MAIN_DEP = 


MAIN_RCDEP = .\algocorp.ico


all:	$(PROJ).EXE $(PROJ).BSC

MAIN.OBJ:	MAIN.C $(MAIN_DEP)
	$(CC) $(CFLAGS) $(CCREATEPCHFLAG) /c MAIN.C

MAIN.RES:	MAIN.RC $(MAIN_RCDEP)
	$(RC) $(RCFLAGS) $(RCDEFINES) -r MAIN.RC


$(PROJ).EXE::	MAIN.RES

$(PROJ).EXE::	MAIN.OBJ $(OBJS_EXT) $(DEFFILE)
	echo >NUL @<<$(PROJ).CRF
MAIN.OBJ +
$(OBJS_EXT)
$(PROJ).EXE
$(MAPFILE)
$(LIBS)
$(DEFFILE);
<<
	link $(LFLAGS) @$(PROJ).CRF
	$(RC) $(RESFLAGS) MAIN.RES $@
	@copy $(PROJ).CRF MSVC.BND

$(PROJ).EXE::	MAIN.RES
	if not exist MSVC.BND 	$(RC) $(RESFLAGS) MAIN.RES $@

run: $(PROJ).EXE
	$(PROJ) $(RUNFLAGS)


$(PROJ).BSC: $(SBRS)
	bscmake @<<
/o$@ $(SBRS)
<<
