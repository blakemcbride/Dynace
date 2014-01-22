# Makefile for Plan 9
</$objtype/mkfile
BINDIR = ../bin
LIBDIR = ../lib
INCDIR = ../include
DPP = $BINDIR/dpp
DPPOPTS = -C
CFLAGS = -DPLAN9 -I$INCDIR -p
%.c : %.d
	$DPP $DPPOPTS -p $stem.d
%.$O : %.c
	$CC $CFLAGS $stem.c
