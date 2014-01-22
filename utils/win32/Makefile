
ADSDIR = \Blake\Systems\ADSW.Subversion

.IF $(DEBUG)
CFLAGS = -nologo -Zi -Od -MT
DLFLAG = /debug
.ELSE
CFLAGS = -nologo -MT
.END

.obj.exe:
	link /nologo $(DLFLAG) $< setargv.obj

all : mv.exe cp.exe rm.exe touch.exe du.exe cmp.exe df.exe xd2.exe tm.exe timec.exe cat.exe dc.exe


rm.exe : rm.c
	cl $(CFLAGS) -I$(ADSDIR)/ads-inc rm.c setargv.obj $(ADSDIR)/lib/adscm.lib

cp.exe : cp.c JmgStat.c
	cl $(CFLAGS) $< setargv.obj

du.exe : du.c
	cl -nologo -O2 $< setargv.obj

cmp.exe : cmp.c
	cl -nologo -O2 $< setargv.obj

df.exe : df.c
	cl -nologo -O2 $<

xd2.exe : xd2.c
	cl -nologo -O2 $< setargv.obj

tm.exe : tm.c
	cl $(CFLAGS) -I$(ADSDIR)/ads-inc tm.c setargv.obj $(ADSDIR)/lib/{console32 adscm}.lib

timec.exe : timec.c
	cl -nologo -O2 $< setargv.obj

cat.exe : cat.c
	cl -nologo -O2 $< setargv.obj

dc.exe : dc.c
	cl $(CFLAGS) -I$(ADSDIR)/ads-inc dc.c setargv.obj $(ADSDIR)/lib/{console32 adscm}.lib

clean:
	rm -zq *.obj *.bak *~ *.ilk *.pdb

