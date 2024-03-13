# Makefile for Plan 9
<mkfile.inc
all:V: setup.p9
	cd kernel   ; mk -s ; cd ..
	cd class    ; mk -s ; cd ..
	cd dpp      ; mk -s install ; cd ..
	cd generics ; mk -s ; cd ..
base:V: makegens
	cd kernel   ; mk ; cd ..
	cd class    ; mk ; cd ..
	cd generics ; mk ; cd ..
debug:V: setup.p9
	cd kernel   ; mk DEBUG=1 ; cd ..
	cd class    ; mk DEBUG=1 ; cd ..
	cd dpp      ; mk DEBUG=1 install ; cd ..
	cd generics ; mk DEBUG=1 ; cd ..
clean:V:
	cd kernel   ; mk clean ; cd ..
	cd class    ; mk clean  ; cd ..
	cd generics ; mk clean ; cd ..
	cd dpp      ; mk clean ; cd ..
	rm -f examples/ex*/*.$O
	rm -f examples/ex*/main
	rm -f bin/*.$O
realclean :V: clean
	rm -f setup.unx setup.dos setup.p9
	rm -f bin/dpp bin/delcr bin/addcr
	rm -f lib/Dynace.a
	rm -f include/direct.h
	rm -f include/errno.h
	rm -f include/fcntl.h
	rm -f include/io.h
	rm -f include/math.h
	rm -f include/stdarg.h
	rm -f include/string.h
	rm -f include/time.h
	rm -f include/unistd.h
	rm -fr include/sys
ship-unix :V: realclean
setup.p9 :
	cd kernel ; $CC -DPLAN9 -I../include plan9.c ; cd ..
	cd bin ; $CC -DPLAN9 -I../include delcr.c ; $LD -o delcr delcr.$O ../kernel/plan9.$O ; cd ..
	cd kernel   ; ../bin/delcr mkfile README *.d ; cd ..
	cd class    ; ../bin/delcr mkfile README *.d ; cd ..
	cd threads  ; ../bin/delcr README *.d ; cd ..
	cd dpp      ; ../bin/delcr mkfile README *.d ; cd ..
#  Times associated with .c files must be after the .d files for make's sake
	sleep 2
	cd kernel   ; ../bin/delcr *.h *.iv *.c ; cd ..
	cd class    ; ../bin/delcr *.h *.c ; cd ..
	cd threads  ; ../bin/delcr *.c ; cd ..
	cd dpp      ; ../bin/delcr *.h ; cd ..
	sleep 2
	cd dpp      ; ../bin/delcr *.c ; cd ..
	cd docs     ; ../bin/delcr *.txt ; cd ..
	cd generics ; ../bin/delcr mkfile README makegens mkgens3 mkgens4 mkgens4f mkgensx ; cd ..
	cd include  ; ../bin/delcr *.h ; cd ..
	cd generics ; chmod 775 makegens mkgens3 mkgens4 mkgens4f mkgensx ; cd ..
	bin/delcr mkfile mkfile.inc examples/ex*/mkfile
	bin/delcr makefile.unx makefile.inc */makefile.unx examples/ex*/makefile.unx
	bin/delcr examples/ex*/*.c
	bin/delcr examples/ex*/*.d
	bin/delcr examples/ex*/readme
	bin/delcr examples/list.txt
	bin/delcr dropunx.sh
	chmod 755 dropunx.sh
	rm -f setup.dos
	touch include/direct.h
	touch include/errno.h
	touch include/fcntl.h
	touch include/io.h
	touch include/math.h
	touch include/stdarg.h
	touch include/string.h
	touch include/time.h
	touch include/unistd.h
	rm -rf include/sys
	mkdir include/sys
	touch include/sys/locking.h
	touch include/sys/stat.h
	touch include/sys/types.h
	rm -rf lib
	mkdir lib
	touch dpp/generics.1
	sleep 1
	touch dpp/*.h
	sleep 1
	touch dpp/*.c
	touch setup.p9
makegens:V:
	cd kernel ; rm -f generics.* ; $DPP $DPPOPTS -h -i -s *.d ; mv generics.h ../include ; cd ..
	cd class ; $DPP $DPPOPTS -h -i -g ../include/generics.h -s *.d ; mv generics.h ../include ; cd ..
	cd threads ; $DPP $DPPOPTS -h -i -g ../include/generics.h -s *.d ; mv generics.h ../include ; cd ..
newgens :V: makegens
	cd kernel   ;  mk reallynewgens ; cd ..
	cd class    ;  mk newgens ; cd ..
	cd threads  ;  mk newgens ; cd ..
	cd dpp      ;  mk newgens  ;  mk  generics.c ; cd ..
