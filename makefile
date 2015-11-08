# Makefile for Linux, Mac, BSD, Cygwin, Solaris, etc.   	-*-Makefile-*-
#  Call this makefile with "WINE=1" if compiling with/for WINE
include makefile.inc
all : setup.unx
	cd kernel   ; $(MAKE) -f makefile
	cd class    ; $(MAKE) -f makefile
	cd threads  ; $(MAKE) -f makefile
	cd dpp      ; $(MAKE) -f makefile install
	cd generics ; $(MAKE) -f makefile
ifdef WINE
	cd Windows  ; $(MAKE) -f makefile
	cd ODBC     ; $(MAKE) -f makefile
endif
base : makegens
	cd kernel   ; $(MAKE) -f makefile
	cd class    ; $(MAKE) -f makefile
	cd threads  ; $(MAKE) -f makefile
	cd generics ; $(MAKE) -f makefile
ifdef WINE
	cd Windows  ; $(MAKE) -f makefile
	cd ODBC     ; $(MAKE) -f makefile
	cd Java/Dynace  ; $(MAKE) -f makefile
endif
debug : setup.unx
	cd kernel   ; $(MAKE) -f makefile DEBUG=1
	cd class    ; $(MAKE) -f makefile DEBUG=1
	cd threads  ; $(MAKE) -f makefile DEBUG=1
	cd dpp      ; $(MAKE) -f makefile DEBUG=1 install
	cd generics ; $(MAKE) -f makefile DEBUG=1
clean :
	cd kernel   ; $(MAKE) $@ -f makefile
	cd class    ; $(MAKE) $@ -f makefile
	cd threads  ; $(MAKE) $@ -f makefile
	cd generics ; $(MAKE) $@ -f makefile
	cd dpp      ; $(MAKE) $@ -f makefile
ifdef WINE
	cd Windows  ; $(MAKE) $@ -f makefile
	cd ODBC     ; $(MAKE) $@ -f makefile
endif
	find . -name '*~' -exec rm \{\} \;
	find . -name '*.o' -exec rm \{\} \;
	find . -name '*.obj' -exec rm \{\} \;
	find . -name '*.tmp' -exec rm \{\} \;
	find . -name '*.{*' -exec rm \{\} \;
	find . -name '--linux-.---' -exec rm - \{\} \;
	find examples -name '*.exe' -exec rm \{\} \;
	find examples -name 'main' -exec rm \{\} \;
realclean : clean
	rm -f setup.unx setup.dos setup.p9
	rm -f bin/dpp
	rm -f bin/dpp.exe
	rm -f bin/delcr
	rm -f bin/delcr.exe
	rm -f bin/addcr
	rm -rf lib/dynace.a lib/wds.a
ship-unix : realclean
	find . -name '*.exe' -exec rm \{\} \;
setup.unx :
	@-mkdir lib
	cd bin ; $(CC) -o delcr -O delcr.c
	cd kernel   ; ../bin/delcr makefile README *.d
	cd class    ; ../bin/delcr makefile README *.d
	cd threads  ; ../bin/delcr makefile README *.d
	cd dpp      ; ../bin/delcr makefile README *.d
#  Times associated with .c files must be after the .d files for make's sake
	sleep 2
	cd kernel   ; ../bin/delcr *.h *.iv *.c
	cd class    ; ../bin/delcr *.h *.c
	cd threads  ; ../bin/delcr *.c
	cd dpp      ; ../bin/delcr *.h
	sleep 2
	cd dpp      ; ../bin/delcr *.c
	cd docs     ; ../bin/delcr  *.DOC *.txt *.tex
	cd generics ; ../bin/delcr makefile README makegens mkgens3 mkgens4 mkgens4f mkgensx
	cd include  ; ../bin/delcr *.h
	cd generics ; chmod 775 makegens mkgens3 mkgens4 mkgens4f mkgensx
	find examples -name 'makefile' -exec bin/delcr \{\} \;
	find examples -name 'readme'       -exec bin/delcr \{\} \;
	find examples -name 'README'       -exec bin/delcr \{\} \;
	find examples -name '*.h'          -exec bin/delcr \{\} \;
	find examples -name '*.d'          -exec bin/delcr \{\} \;
	find examples -name '*.c'          -exec bin/delcr \{\} \;
	bin/delcr examples/list makefile makefile.inc
#	find multhead -type f -exec bin/delcr \{\} \;
	rm -f setup.dos
	touch setup.unx
setup.dos :
	cd bin ; $(CC) -o addcr -O addcr.c
	cd include  ; ../bin/addcr *.h
	cd kernel   ; ../bin/addcr  README *.d *.h *.iv *.c
	cd class    ; ../bin/addcr  README *.d *.h *.c
	cd threads  ; ../bin/addcr  README *.d     *.c
	cd dpp      ; ../bin/addcr  README *.d *.h *.c
	cd docs     ; ../bin/delcr *
	cd generics ; ../bin/addcr  README
	find examples -name 'readme'       -exec bin/addcr \{\} \;
	find examples -name 'README'       -exec bin/addcr \{\} \;
	find examples -name '*.h'          -exec bin/addcr \{\} \;
	find examples -name '*.d'          -exec bin/addcr \{\} \;
	find examples -name '*.c'          -exec bin/addcr \{\} \;
	bin/addcr examples/list
	rm -f setup.unx
	touch setup.dos
makegens:
	cd kernel ; rm -f generics.* ; $(DPP) $(DPPOPTS) -h -i -s *.d ; mv generics.h ../include
	cd class ; $(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d ; mv generics.h ../include
	cd threads ; $(DPP) $(DPPOPTS) -h -i -g ../include/generics.h -s *.d ; mv generics.h ../include
ifdef WINE
	cd Windows  ; $(MAKE) -f makefile newgens
	cd ODBC     ; $(MAKE) -f makefile newgens
	cd Java/Dynace  ; $(MAKE) -f makefile newgens
endif
newgens : makegens
	cd kernel   ;  $(MAKE) -f makefile reallynewgens
	cd class    ;  $(MAKE) -f makefile newgens
	cd threads  ;  $(MAKE) -f makefile newgens
	cd dpp      ;  $(MAKE) -f makefile newgens  ;  $(MAKE) -f makefile generics.c
ifdef WINE
	cd Windows  ; $(MAKE) -f makefile newgens
	cd ODBC     ; $(MAKE) -f makefile newgens
	cd Java/Dynace  ; $(MAKE) -f makefile newgens
endif
# The following target is used to convert an SVN checkout into a shippable distribution
# It must only be run immediatly after a co or export
# It creates a system which can boot without DPP, Windows or WINE
bootable:
	rm -f include/generics.h
	cp bin/Linux/dpp bin
	chmod 755 bin/dpp
	$(MAKE) -f makefile STRAT=-S2 newgens
	touch  ODBC/sqlgrammar.c ODBC/sqlgrammar.h ODBC/sqltokens.c
