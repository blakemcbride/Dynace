#  Makefile designed for use by the GNUMAKE utility and		-*-Makefile-*-
#  the Wine libraries

WINE = 1

include ../makefile.inc


DPPOPTS += -g $(INCDIR)/generics.h


YACC = bison
LEX = flex

CFLAGS += -I$(WINEINCPATH)/msvcrt -fno-builtin


WINELIBS = -lkernel32 -luser32 -lgdi32 -lcomdlg32 -lcomctl32 -ladvapi32 -lshell32 -lmpr -lwine -lmsvcrt -lmsimg32

DEBUG = -g
#DEBUG = -O

TARGET = main

CLASSES =  Database.d Statement.d StatementInfo.d TableInfo.d ColumnInfo.d TableListbox.d VirtualListbox.d WorkFlow.d \
	WorkFlowAssociation.d ODBCListbox.d GenericSelection.d TreeViewWorkFlow.d ListBoxWorkFlow.d StringFile.d \
        TransactionProcessing.d TaskListDetail.d TaskListDetailRecord.d UniqueIdentifier.d CacheResult.d CacheCalculation.d \
	CachedDatabase.d CacheStatement.d CachedTable.d CacheStatementInfo.d

CFILES = prikeys.c sqlparser.c sqltokens.c sqlgrammar.c

OBJS = $(CFILES:.c=.o) $(CLASSES:.d=.o)

%.res : %.rc
	echo '#include <winuser.h>' >temp.rc
	cat $< >>temp.rc
	wrc temp.rc
	rm -f temp.rc
	mv temp.res $@

%.o : %.c
	$(CC) $(CFLAGS) $(DEBUG) -c $<

allok : generics.1 curlib
	echo Done >$@

curlib :  $(OBJS)
	$(AR) $(ARFLAGS) $(LIBDIR)/wds.a $?
	@if [ -f /usr/bin/ranlib -o -f /bin/ranlib ];\
	then \
		echo ranlib $(LIBDIR)/wds.a;\
		ranlib $(LIBDIR)/wds.a;\
	fi
	echo Done >$@

$(TARGET) : generics.1 generics.h generics.o $(OBJS) main.res
	$(CC) $(DEBUG) -o $(TARGET) $(OBJS) generics.o main.res $(LIBDIR)/wds.a $(LIBDIR)/dynace.a $(WINELIBS) -lm 

sqlgrammar.c sqlgrammar.h:	sqlgrammar.y sqlstructs.h
	$(YACC) -tvd sqlgrammar.y
	mv sqlgrammar.tab.h sqlgrammar.h
	mv sqlgrammar.tab.c sqlgrammar.c
	mv sqlgrammar.output sqlgrammar.out

sqlparser.o : sqlparser.c sqlgrammar.c sqlstructs.h

sqltokens.c:	sqltokens.l sqlgrammar.h
	$(LEX) -i sqltokens.l
	mv lex.yy.c sqltokens.c

sqltokens.o : sqltokens.c
	$(CC) -c $(CFLAGS) -D_CRT_SECURE_NO_DEPRECATE -D_POSIX_ $<

sqlgrammar.o : sqlgrammar.c
	$(CC) -c $(CFLAGS) -D_CRT_SECURE_NO_DEPRECATE -D_POSIX_ $<

generics.c : generics.h
	$(BINDIR)/dpp -g -c

# generics.c must be compiled with gcc and not winegcc!
generics.o : generics.c 
	$(CC) $(GFLAGS) -c $<

generics.1 newgens reallynewgens:
	$(DPP) $(DPPOPTS) -p $(CLASSES) -h $(INCDIR)/generics.h
	touch generics.1

clean:
	-rm -f *.o *.exe generics.* *.o *~ main.res temp.rc $(CLASSES:.d=.c) sqlgrammer.out
	-rm -f curlib allok
#	-rm -f sqlgrammer.c sqlgrammer.h sqlgrammer.out sqltokens.c

realclean: clean
	-rm -f $(TARGET) $(TARGET).exe.so
