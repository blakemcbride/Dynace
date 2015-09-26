



CC = cl -nologo

JSDK =  $(JAVA_HOME)  #  \j2sdk1.4.1_01


.IF $(DEBUG)
CFLAGS = /MTd /W3 /Gm /EHsc /ZI /Od /I $(JSDK)\include /I $(JSDK)\include\win32 /D WIN32 /D _DEBUG /D _WINDOWS /D _MBCS /D _USRDLL /D JAVADYNACE_EXPORTS /FpJavaDynace.pch /FD /c
LFLAGS = /nologo /dll /incremental:yes /pdb:$*.pdb /debug /machine:I386
.ELSE
CFLAGS = /MT /W3 /EHsc /O2 /I $(JSDK)\include /I $(JSDK)\include\win32 /D WIN32 /D NDEBUG /D _WINDOWS /D _MBCS /D _USRDLL /D JAVADYNACE_EXPORTS /FpJavaDynace.pch /FD /c
LFLAGS = /nologo /dll /incremental:no  /machine:I386 
.END

.cpp.obj:
	$(CC) $(CFLAGS) $<

JavaDynace.dll : JavaDynace.obj
	link $(LFLAGS) /out:$@ /implib:$*.lib $<

JavaDynace.obj : Dynace_DynaceBase.h

Dynace_DynaceBase.h : ../Java/DynaceBase.java
	$(JSDK)\bin\javac  -d . ../java/*.java
	$(JSDK)\bin\javah -jni -classpath . Dynace.DynaceBase
	$(BINDIR)\rm -rf Dynace

clean:
	rm -zq *.obj *.pch *.pdb *.plg *.sbr *.ncb *~ *.bak *.dep *.exp *.ilk *.opt *.idb Dynace_DynaceBase.h
	rm -rfz Dynace

realclean: clean
	rm -zq *.dll *.lib
