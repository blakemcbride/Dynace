BINDIR = ..\..\bin


wdsexam08.class : wdsexam08.java
	javac -classpath ..\Java\Dynace.jar $<

clean realclean :
	$(BINDIR)\rm -zq *.class *.bak *~
