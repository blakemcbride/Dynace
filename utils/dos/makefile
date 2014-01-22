
CFLAGS = -nologo -AS -c

all : mv.exe cp.exe rm.exe touch.exe

mv.exe : mv.obj
	link /BATCH mv setargv /NOI /NOE ;

cp.exe : cp.obj
	link /BATCH cp setargv,,nul.map,/NOI/NOE/STACK:8000;

cp.obj : cp.c
	cl -AL -c -nologo cp.c

rm.exe : rm.obj
	link /BATCH rm setargv /NOI /NOE;

touch.exe : touch.obj
	link /BATCH touch setargv /NOI /NOE;

