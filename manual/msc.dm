
.tex.dvi .PRECIOUS:
	tex $<
	texindex $*.fn
	tex $<

.dvi.pdf :
	dvipdfm $*

.dvi.ps:
	dvips -T Letter -O 0in,-0.8in $*

all: Dynace.pdf

Dynace.pdf : Dynace.dvi

Dynace.dvi : Dynace.tex man1.tex man2.tex man3.tex man4.tex man5.tex classes1.tex \
	Array.tex \
	Association.tex \
	BitArray.tex \
	BTree.tex \
	BTreeNode.tex \
	Character.tex \
	CharacterArray.tex \
	Constant.tex \
	Date.tex \
	DateTime.tex \
	Dictionary.tex \
	DoubleFloat.tex \
	DoubleFloatArray.tex \
	File.tex \
	FindFile.tex \
	FloatArray.tex \
	IntegerArray.tex \
	IntegerAssociation.tex \
	IntegerDictionary.tex \
	Link.tex \
	LinkList.tex \
	LinkObject.tex \
	LinkObjectSequence.tex \
	LinkSequence.tex \
	LinkValue.tex \
	LongArray.tex \
	LongInteger.tex \
	LookupKey.tex \
	LowFile.tex \
	Number.tex \
	NumberArray.tex \
	ObjectArray.tex \
	ObjectAssociation.tex \
	ObjectPool.tex \
	Pipe.tex \
	Pointer.tex \
	PointerArray.tex \
	PropertyList.tex \
	Semaphore.tex \
	Sequence.tex \
	Set.tex \
	SetSequence.tex \
	ShortArray.tex \
	ShortInteger.tex \
	Socket.tex \
	Stream.tex \
	String.tex \
	StringAssociation.tex \
	StringDictionary.tex \
	Thread.tex \
	Time.tex \
	UnsignedShortArray.tex \
	UnsignedShortInteger.tex

clean:
	$(BINDIR)\rm -zq s-*.tex
	$(BINDIR)\rm -zq *.cp *.fn *.fns *.ky *.pg *.toc *.tp *.vr *.aux *.log
	$(BINDIR)\rm -zq *~ *.bak

realclean: clean
	$(BINDIR)\rm -zq *.dvi *.ps *.pdf


