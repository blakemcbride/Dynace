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




defclass CacheResult {
	iCalculations;
 class:
	cTables;		//  StringDictionary of table names (table name, StringDictionary of column names)
	                        //        StringDictionary of column names (column name, ObjectArray of refering CasheResult instances)
	cInstances;		//  Set of instances
 init:	init_class;
};

#include <ctype.h>


static	char	*downcase(char *t, char *f);

static	void	init_class()
{
	cInstances = gNew(Set);
	cTables = gNew(StringDictionary);
}

private	imeth	init()
{
	iCalculations = gNew(LinkList);
	return self;
}

cmeth	gNew()
{
	object	r = init(gNew(super));
	gAdd(cInstances, r);
	return r;
}

imeth	gAdd(calc)
{
	gAddFirst(iCalculations, gNewWithObj(LinkValue, calc));
	return self;
}

imeth	gRemoveObj(calc)
{
	object	seq, lnk;

	for (seq=gSequence(iCalculations) ; lnk = gNext(seq) ; )
		if (calc == gValue(lnk)) {
			gDispose(lnk);
			gDispose(seq);
			break;
		}
	return self;
}

private	imeth	void	delete_references()
{
	object	seq, sa, seq2, sa2, col, *p;
	int	sz, i;

	for (seq=gSequence(cTables) ; sa = gNext(seq) ; )
		for (seq2=gSequence(gValue(sa)) ; sa2 = gNext(seq2) ; ) {
			col = gValue(sa2);
			sz = gSize(col);
			p = (object *) gArrayPointer(col);
			for (i=0 ; i < sz ; i++)
				if (p[i] == self) {
					p[i] = NULL;
					break;
				}
		}
}

imeth	gDispose, gDeepDispose ()
{
	delete_references(self);
	gRemoveObj(cInstances, self);
	gDeepDispose(iCalculations);
	return gDispose(super);
}

imeth	gMonitorColumn(char *table, char *col)
{
	object	t, c, *p;
	char	tc[128], cc[128];
	int	sz, i, found;

	t = gFindValueStr(cTables, downcase(tc, table));
	if (!t)
		gAddStr(cTables, tc, t=gNew(StringDictionary));
	c = gFindValueStr(t, downcase(cc, col));
	if (!c)
		gAddStr(t, cc, c=vNew(ObjectArray, 1, 15));
		p = (object *) gArrayPointer(c);
		sz = gSize(c);
		for (found=i=0 ; i < sz ; i++)
			if (p[i] == self) {
				found = 1;
				break;
			}
		if (!found)
			for (i=0 ; i < sz ; i++)
				if (!p[i]) {
					p[i] = self;
					found = 1;
					break;
				}
		if (!found) {
			int	newsz = sz + 5;
			vReshape(c, 1, newsz);
			p = (object *) gArrayPointer(c);
			p[sz] = self;
			for (i=sz+1 ; i < newsz ; i++)
				p[i] = NULL;
		}
	return self;
}

cmeth	gGetTable(char *table)
{
	char	tc[128];
	return gFindValueStr(cTables, downcase(tc, table));
}

imeth	gInvalidate()
{
	object	seq, lnk;

	for (seq=gSequence(iCalculations) ; lnk = gNext(seq) ; )
		gInvalidate(gValue(lnk));
	return self;
}

cmeth	gInvalidateAll()
{
	object	seq, obj;

	if (cInstances)
		for (seq=gSequence(cInstances) ; obj = gNext(seq) ; )
			gInvalidate(obj);
	return self;
}

cmeth	gInvalidateResultCache(trc, cache)
{
	object	*p = gArrayPointer(cache);
	int	i, sz = gSize(cache);
	for (i=0 ; i < sz ; i++)
		if (p[i])
			gInvalidate(p[i]);
	return self;
}

imeth	int	gHasReferences()
{
	object	seq, sa, seq2, sa2, col, *p;
	int	sz, i, r=0;

	for (seq=gSequence(cTables) ; sa = gNext(seq) ; ) {
		for (seq2=gSequence(gValue(sa)) ; sa2 = gNext(seq2) ; ) {
			col = gValue(sa2);
			sz = gSize(col);
			p = (object *) gArrayPointer(col);
			for (i=0 ; i < sz ; i++)
				if (p[i] == self) {
					r++;
					break;
				}
			if (r) {
				gDispose(seq2);
				break;
			}
		}
		if (r) {
			gDispose(seq);
			break;
		}
	}
	return r;
}

static	char	*downcase(char *t, char *f)
{
	char	c, *b = t;
	while (*f) {
		c = *f++;
		*t++ = tolower(c);
	}
	*t = '\0';
	return b;
}





