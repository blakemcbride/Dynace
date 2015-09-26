#  Makefile designed for use by the DMAKE.EXE utility and
#  Microsoft Visual C/C++ 32 bit


TARGET = main


C_SRC = prikeys.c sqlparser.c sqltokens.c sqlgrammar.c


CLASS_SRC = Database.d Statement.d StatementInfo.d TableInfo.d ColumnInfo.d TableListbox.d VirtualListbox.d WorkFlow.d \
	WorkFlowAssociation.d ODBCListbox.d GenericSelection.d TreeViewWorkFlow.d ListBoxWorkFlow.d StringFile.d \
        TransactionProcessing.d TaskListDetail.d TaskListDetailRecord.d UniqueIdentifier.d CacheResult.d CacheCalculation.d \
	CachedDatabase.d CacheStatement.d CachedTable.d CacheStatementInfo.d


RESOURCES = $(TARGET).res

YACC = bison
LEX = flex

BINDIR = ..\bin
LIBDIR = ..\lib
INCDIR = ..\include

CFEXTRA += -MT

.IF $(NATIVE_THREADS)
CFEXTRA += -DNATIVE_THREADS
.END

.IF	$(DEBUG)
CFLAGS = -nologo -I$(INCDIR) -WX -D_WINDOWS -Od -Zi -Fd$(LIBDIR)\dynm32.pdb $(CFEXTRA)
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS -Zi -Fd$(LIBDIR)\dynm32.pdb
LFLAGS = /nologo /subsystem:windows /debug
.ELSE
CFLAGS = -nologo -I$(INCDIR) -WX -O2 -D_WINDOWS $(CFEXTRA)
GFLAGS = -nologo -I$(INCDIR) -Oityb1 -Gs -Gy -D_WINDOWS
LFLAGS = /nologo /subsystem:windows
.END

.IF $(DEMO)
CFLAGS += -DDEMO
.ENDIF



LIBS = $(LIBDIR)\{dwdsnm dynlcm}.lib

MSLIBS = {kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 odbc32 winmm ole32}.lib


OBJS = {$(CLASS_SRC:b)}.obj {$(C_SRC:b)}.obj sqlgrammar.obj

CC = cl

.d.c .PRECIOUS :
	$(BINDIR)\dpp -g $(INCDIR)\generics.h -p $<


# Since DMAKE diversions treat the backslach like an escape sequence it is
# necessary to double up the backslashes located in paths
LIBS2     := $(LIBS:s/\/\\/)
OBJS2     := $(OBJS:s/\/\\/)


.rc.res:
	rc $<

allok.nm .LIBRARY : $(INCDIR)\generics.h curlib.nm
	echo Done >$@

$(TARGET).exe : curlib.nm main.obj $(OBJS) $(RESOURCES) $(LIBS)
	link @$(mktmp /out:$@ $(LFLAGS)\nmain.obj\n\
		$(LIBS2)\n$(MSLIBS)\n$(RESOURCES)\n)


curlib.nm .LIBRARY : $(OBJS)
.IF  $(NEW)
	lib /nologo /out:$(LIBDIR)\dwdsnm.lib @$(mktmp $(?:t"\n")\n)
.ELSE
	lib /nologo /out:$(LIBDIR)\dwdsnm.lib $(LIBDIR)\dwdsnm.lib @$(mktmp $(?:t"\n")\n)
.END
	$(BINDIR)\rm -zq *.obj
	echo Done >$@

main.obj : resource.h

tp.obj : tp.c tp.h
	$(CC) $(CFLAGS) -MT -c tp.c

StringFile.obj : StringFile.c
#	$(CC) $(CFLAGS) -MT -c $<

$(INCDIR)\generics.h : $(CLASS_SRC)
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -t $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(^:t"\n"))

generics.c : generics.h
	$(BINDIR)\dpp $(STRAT) -C -g -c -Isc windows.h sql.h sqlext.h

generics.obj : generics.c
	$(CC) -c $(GFLAGS) $<

newgens : 
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -h $(INCDIR)\generics.h -p @$(mktmp $(CLASS_SRC:t"\n")\n)

makegens:
	$(BINDIR)\dpp $(STRAT) -C -g $(INCDIR)\generics.h -h $(INCDIR)\generics.h -s @$(mktmp $(CLASS_SRC:t"\n")\n)

sqlgrammar.c sqlgrammar.h:	sqlgrammar.y sqlstructs.h
	${YACC} -tvd sqlgrammar.y
	mv sqlgrammar.tab.h sqlgrammar.h
	mv sqlgrammar.tab.c sqlgrammar.c
	mv sqlgrammar.output sqlgrammar.out

sqlparser.obj : sqlparser.c sqlgrammar.c sqlstructs.h

sqltokens.c:	sqltokens.l sqlgrammar.h
	${LEX} -i sqltokens.l
	mv lex.yy.c sqltokens.c

sqltokens.obj : sqltokens.c
	$(CC) -c $(CFLAGS) -D_CRT_SECURE_NO_DEPRECATE -D_POSIX_ $<

sqlgrammar.obj : sqlgrammar.c
	$(CC) -c $(CFLAGS) -D_CRT_SECURE_NO_DEPRECATE -D_POSIX_ $<

	
clean:
	$(BINDIR)\rm -zq sqlgrammar.out
	$(BINDIR)\rm -zq *.obj
	$(BINDIR)\rm -zq *.exe
	$(BINDIR)\rm -zq *.ex1
	$(BINDIR)\rm -zq *.res
	$(BINDIR)\rm -zq *.aps
	$(BINDIR)\rm -zq *.pdb
	$(BINDIR)\rm -zq *.wsp
	$(BINDIR)\rm -zq *.ilk
	$(BINDIR)\rm -zq *.bsc
	$(BINDIR)\rm -zq *.vcp
	$(BINDIR)\rm -zq *.pch *.idb
	$(BINDIR)\rm -zq *.sbr
	$(BINDIR)\rm -zq *.vcw
	$(BINDIR)\rm -zq *.map *.ncb
	$(BINDIR)\rm -zqr WinDebug
	$(BINDIR)\rm -zqr WinRel
	$(BINDIR)\rm -zq *.~ *.?~ *.??~ *.{* *.bak #*.*
#	$(BINDIR)\rm -zq sqlgrammar.c
#	$(BINDIR)\rm -zq sqlgrammar.h
#	$(BINDIR)\rm -zq sqltokens.c


realclean: clean
	$(BINDIR)\rm -zq curlib.* allok.* generics.c generics.h generics.1
	$(BINDIR)\rm -zq {$(CLASS_SRC:b)}.c

