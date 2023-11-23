
include ../makefile.inc

DPPOPTS += -g $(INCDIR)/generics.h

CLASSES = LinkList.d Link.d LinkValue.d LinkObject.d ShortInteger.d Association.d \
	LookupKey.d Set.d Dictionary.d String.d LongInteger.d DoubleFloat.d \
	Character.d SetSequence.d LinkObjectSequence.d StringAssociation.d StringDictionary.d LinkSequence.d \
	Date.d Pointer.d Number.d ObjectAssociation.d UnsignedShortInteger.d \
	Sequence.d Stream.d File.d LowFile.d Array.d CharacterArray.d XMLNode.d \
	Constant.d ShortArray.d UnsignedShortArray.d IntegerArray.d LongArray.d FloatArray.d \
	DoubleFloatArray.d BitArray.d ObjectArray.d PointerArray.d NumberArray.d IntegerAssociation.d \
	IntegerDictionary.d BTree.d BTreeNode.d Memory.d crc.d Time.d DateTime.d ObjectPool.d \
	PropertyList.d WeakPointer.d BufferedTempFile.d Socket.d Logger.d \
	Abort.d Condition.d Error.d Restart.d SeriousCondition.d SimpleRestart.d SimpleWarning.d Warning.d

WINCLASSES = FindFile.d memalloc.d MimeBodyPart.d MultipartMime.d

OBJ = $(CLASSES:.d=.o) memalloc.o


#allok.unx :  $(INCDIR)/generics.h $(LIBDIR)/dynace.a
#	echo Done >$@

$(LIBDIR)/dynace.a :  $(OBJ)
	$(AR) $(ARFLAGS) $@ $?
	@if [ -f /usr/bin/ranlib -o -f /bin/ranlib ];\
	then \
		echo ranlib $@;\
		ranlib $@;\
	fi

$(INCDIR)/generics.h : $(CLASSES)
	$(DPP) $(DPPOPTS) -t $(INCDIR)/generics.h -h $(INCDIR)/generics.h -p $^


Dictionary.o Set.o SetSequence.o StringDictionary.o IntegerDictionary.o : set1.h


String.o Array.o : memalloc.h

Array.o : array2.h

Array.o CharacterArray.o  ShortArray.o UnsignedShortArray.o LongArray.o FloatArray.o \
	DoubleFloatArray.o BitArray.o ObjectArray.o PointerArray.o: array1.h

memalloc.c : memalloc.d
	cp memalloc.d memalloc.c

newgens:
	$(DPP) $(DPPOPTS) -p $(CLASSES) $(WINCLASSES) -h $(INCDIR)/generics.h

clean:
	-rm -f *.obj *.exe generics.* main *.o core *~ *.pdb allok.unx


