
JSDK =  $(JAVA_HOME)  #  \j2sdk1.4.1_01


BINDIR = ..\..\bin

Dynace.jar : DynaceBase.java DynaceClass.java DynaceGeneric.java IObjectFactory.java DynaceObject.java
	$(BINDIR)\rm -rfzq Dynace.jar Dynace
	-+md Dynace
	-+md Dynace\generics
	$(BINDIR)\cp *.java Dynace
	$(BINDIR)\mv Dynace\DynaceObject.java Dynace\Dynace.java Dynace\generics
	$(JSDK)\bin\javac -classpath . Dynace/*.java Dynace/generics/*.java
	$(JSDK)\bin\jar cvf Dynace.jar Dynace/*.class Dynace/generics/*.class
#	$(BINDIR)\rm -rfzq Dynace

DynaceObject.java : ..\..\include\generics.h
	$(BINDIR)\dpp -g $< -j Dynace.generics

clean:
	$(BINDIR)\rm -zqr *~ *.bak LoadDynClasses.c DynaceObject.java Dynace.java Dynace

realclean: clean
	$(BINDIR)\rm -zq Dynace.jar
