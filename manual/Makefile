
%.dvi : %.tex
	tex $<
	texindex $*.fn
	tex $<

%.pdf : %.dvi
	dvipdfm $*

%.ps : %.dvi
	dvips -T Letter -O 0in,-0.8in $*

all: Dynace.pdf WDS.pdf odbcusr.pdf s-odbcusr.pdf s-WDS.pdf

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



WDS.pdf : WDS.dvi

WDS.dvi : WDS.tex wds1.tex wds2.tex wds3.tex wds4.tex wdscls1.tex

s-WDS.pdf : s-WDS.dvi

s-WDS.dvi : s-WDS.tex s-wds1.tex s-wds2.tex s-wds3.tex s-wds4.tex wdscls1.tex

odbcusr.pdf : odbcusr.dvi

odbcusr.dvi : odbcusr.tex odbcusr1.tex

s-odbcusr.pdf : s-odbcusr.dvi

s-odbcusr.dvi : s-odbcusr.tex s-odbcusr1.tex

s-odbcusr.tex: odbcusr.tex
	sed -f script2 $< >$@


s-odbcusr1.tex: odbcusr1.tex
	sed -f script $< >$@


s-WDS.tex: WDS.tex
	sed -f script3 $< >$@

s-wds1.tex: wds1.tex
	sed -f script $< >$@

s-wds2.tex: wds2.tex
	sed -f script $< >$@

s-wds3.tex: wds3.tex
	sed -f script $< >$@

s-wds4.tex: wds4.tex
	sed -f script $< >$@

clean:
	rm -f s-*.tex
	rm -f *.cp *.fn *.fns *.ky *.pg *.toc *.tp *.vr *.aux *.log
	rm -f *~ *.bak

realclean: clean
	rm -f *.dvi *.ps *.pdf


