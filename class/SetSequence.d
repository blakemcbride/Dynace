/*
  Copyright (c) 1996 Blake McBride
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/





#include "set1.h"


defclass  SetSequence : Sequence  {
	int	iTabidx;
	int	iSize;
	NODE	*iTab;
	NODE	iCurrent;
};

cmeth	gNewSetSeq, <vNew> (int size, int nelm, void *tab)
{
	object	seq = gNew(super);
	ivType	*iv = ivPtr(seq);
	iSize = size;
	iTab = (NODE *) tab;
	if (nelm)  {
		for (; !iTab[iTabidx] ; ++iTabidx);
		iCurrent = iTab[iTabidx];
	}
	return seq;
}

imeth	gNext()
{
	NODE	n;
	if (n = iCurrent)  {
		if (!(iCurrent = iCurrent->next))
			while (++iTabidx < iSize)
				if (iTab[iTabidx])  {
					iCurrent = iTab[iTabidx];
					break;
				}
		return n->luk;
	}
	return gDispose(self);
}





