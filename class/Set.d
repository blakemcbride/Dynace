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




#include <string.h>
#include <math.h>

#include "set1.h"


defclass  Set  {
	int	iSize;		/*  size of iTab		*/
	int	iNelm;		/*  number of entries in iTab	*/
	NODE	*iTab;		/*  the hash table		*/
	CRITICALSECTION	iCS;	/*  in support of native threads  */
class:
	NODE	FNODES;		/*  free node structures	*/
	CRITICALSECTION	cCS;	/*  in support of native threads  */
 init:	class_init;
};

#ifdef	PROFILE
#undef	PMETHOD
#define	PMETHOD
#endif


LOCAL	objrtn	Set_Lookup(object self, object luk, int mode, int deep, int type, object value);

#define LTYPE	0	/*  lookup type		*/

#define NODE_BLOCK_SIZE		50

LOCAL	NODE	new_node(void)
{
	int	i;
	volatile  NODE	r;

	ENTERCRITICALSECTION(cCS);
	if (!FNODES)  {
		FNODES = Tncalloc(struct NODES, NODE_BLOCK_SIZE);
		for (i=0 ; i != (NODE_BLOCK_SIZE-1) ; ++i)
			FNODES[i].next = FNODES + (i + 1);
		gRegisterMemory(Dynace, FNODES, (long)(sizeof(struct NODES) * NODE_BLOCK_SIZE));
	}
	r = FNODES;
	FNODES = FNODES->next;
	LEAVECRITICALSECTION(cCS);
	return r;
}

LOCAL	void	free_node(NODE i)
{
	ENTERCRITICALSECTION(cCS);
	i->luk = NULL;
	i->next = FNODES;
	FNODES = i;
	LEAVECRITICALSECTION(cCS);
}

cmeth	gNewWithInt, <vNew> : Set_New (int size)
{
	object	set = gNew(super);
	ivType	*iv = ivPtr(set);
	iSize = size;
	iTab = Tncalloc(NODE, size);
	INITIALIZECRITICALSECTION(iCS);
	return set;
}

cmeth	gNew()
{
	return Set_New(self, 51);
}

private	imeth	Set_copy(object self, int deep)
{
	object	nobj;
	ivType	*iv2;
	int	i;

	ENTERCRITICALSECTION(iCS);
	if (deep < 2)
		nobj = gCopy(super);
	else
		nobj = gDeepCopy(super);
	iv2 = ivPtr(nobj);

	memset(&iv2->iCS, 0, sizeof iCS);
	INITIALIZECRITICALSECTION(iv2->iCS);
	
	iv2->iSize = iSize;
	iv2->iTab = Tncalloc(NODE, iv2->iSize);
	for (i=0 ; i != iv2->iSize ; ++i)  {
		NODE	n, n2, p;

		for (n=iTab[i], p=NULL ; n ; p=n2, n=n->next)  {
			n2 = new_node();
			switch (deep)  {
			case 0:
			default:
				n2->luk = n->luk;
				break;
			case 1:
				n2->luk = gCopy(n->luk);
				break;
			case 2:
				n2->luk = gDeepCopy(n->luk);
				break;
			}
			n2->next = NULL;
			if (p)
				p->next = n2;
			else
				iv2->iTab[i] = n2;
		}
	}
	LEAVECRITICALSECTION(iCS);
	return nobj;
}

imeth	gCopy()
{
	return Set_copy(self, ClassOf(self) != CLASS);
}

imeth	gDeepCopy()
{
	return Set_copy(self, 2);
}



/* returns the number of elements in the hash table	*/

imeth	int	gSize()
{
	return iNelm;
}

private	imeth	object	Set_delete(object self, int mode, int entire)
{
	int	i;
	
	ENTERCRITICALSECTION(iCS);
	for (i=0 ; i != iSize ; ++i)  {
		NODE	n, p = iTab[i];
		
		for (; p ; p = n)  {
			if (mode == 1)
				gDispose(p->luk);
			else if (mode == 2)
				gDeepDispose(p->luk);
			n = p->next;
			free_node(p);
		}
		iTab[i] = NULL;
	}
	if (entire)  {
		free(iTab);
		LEAVECRITICALSECTION(iCS);
		DELETECRITICALSECTION(iCS);
		self = gDispose(super);
	} else {
		iNelm = 0;
		LEAVECRITICALSECTION(iCS);
	}
	return self;
}	

imeth	object	gDispose, gGCDispose ()
{
	return Set_delete(self, 0, 1);
}

imeth	object	gDeepDispose()
{
	return Set_delete(self, 2, 1);
}

imeth	object	gDispose1()
{
	return Set_delete(self, 1, 1);
}

imeth	object	gDisposeAllNodes()
{
	return Set_delete(self, 0, 0);
}

imeth	object	gDeepDisposeAllNodes()
{
	return Set_delete(self, 2, 0);
}

imeth	object	gDisposeAllNodes1()
{
	return Set_delete(self, 1, 0);
}

/*  execute fun for every key in the table until fun returns non-NULL	*/

imeth	gForAll(object (*fun) (/* ??? */))
{
	int	i;
	
	ENTERCRITICALSECTION(iCS);
	for (i=0 ; i != iSize ; ++i)  {
		NODE	p = iTab[i], n;
		object	v;
		
		for (; p ; p = n) {
			n = p->next;
			if (v = (*(object (*)(object))fun)(p->luk)) {
				LEAVECRITICALSECTION(iCS);
				return(v);
			}
		}
	}
	LEAVECRITICALSECTION(iCS);
	return NULL;
}

imeth	gFirst()
{
	int	i;
	object	r;
	
	ENTERCRITICALSECTION(iCS);
	for (i=0 ; i != iSize ; ++i)
		if (iTab[i]) {
			r = iTab[i]->luk;
			LEAVECRITICALSECTION(iCS);
			return r;
		}
	LEAVECRITICALSECTION(iCS);
	return NULL;
}

imeth	gStringRep()
{
	int	i;
	object	s, t;
	
	s = gStringRepValue(super);
	gAppend(s, (object) "  [\n");
	for (i=0 ; i != iSize  &&  !iTab[i] ; ++i);
	while (i != iSize)  {
		NODE	p = iTab[i];
		
		while (++i != iSize  &&  !iTab[i]);
		while (p)  {
			t = gStringRepValue(p->luk);
			if ((p = p->next)  ||  i != iSize)
				vBuild(s, NULL, "\t", t, ",\n", NULL);
			else
				vBuild(s, NULL, "\t", t, "\n", NULL);
			gDispose(t);
		}
	}
	gAppend(s, (object) "]\n");
	return s;
}

/*  add a new luk if it doesnt already exist - 
    return NULL if it previously existed
   */

imeth	gAdd, <vAdd> (luk)
{
	ChkArg(luk, 2);
	return Set_Lookup(self, luk, HT_ADD, 0, LTYPE, NULL);
}

/*  find an existing key  (return NULL if can't find)	*/

imeth	gFind, <vFind> (luk)
{
	ChkArg(luk, 2);
	return Set_Lookup(self, luk, HT_FIND, 0, LTYPE, NULL);
}

/* find a key - if doesn't exist add  */

imeth	gFindAdd, <vFindAdd> (luk)
{
	ChkArg(luk, 2);
	return Set_Lookup(self, luk, HT_FINDADD, 0, LTYPE, NULL);
}

/*  dispose of a node - if it doesnt exist return NULL  */

imeth	gRemoveObj, <vRemove> (luk)
{
	ChkArg(luk, 2);
	return Set_Lookup(self, luk, HT_DELETE, 0, LTYPE, NULL);
}

/*  dispose of a node - if it doesnt exist return NULL  */

imeth	gDisposeObj(luk)
{
	ChkArg(luk, 2);
	return Set_Lookup(self, luk, HT_DELETE, 1, LTYPE, NULL);
}

/*  deep dispose of a node - if it doesnt exist return NULL  */

imeth	gDeepDisposeObj(luk)
{
	ChkArg(luk, 2);
	return Set_Lookup(self, luk, HT_DELETE, 2, LTYPE, NULL);
}

/* dispose of a group of entries	*/

private	imeth	Set_deleteGroup(object self, int (*fun) (/* ??? */), int deep)
{
	int	i;
	NODE	n, lastp, p;
	
	ENTERCRITICALSECTION(iCS);
	for (i=0 ; i != iSize ; ++i)  {
		for (lastp=NULL, p=iTab[i] ; p ; p=n) {
			n = p->next;
			if ((*(int (*)(object))fun)(p->luk))  {
				if (deep == 1)
					gDispose(p->luk);
				else if (deep == 2)
					gDeepDispose(p->luk);
				if (lastp)
					lastp->next = n;
				else
					iTab[i] = n;
				free_node(p);
				--iNelm;
			}  else
				lastp = p;
		}
	}
	LEAVECRITICALSECTION(iCS);
	return self;
}

imeth	gGroupRemove(int (*fun) (/* ??? */))
{
	return Set_deleteGroup(self, fun, 0);
}

imeth	gDisposeGroup(int (*fun) (/* ??? */))
{
	return Set_deleteGroup(self, fun, 1);
}

imeth	gDeepDisposeGroup(int (*fun) (/* ??? */))
{
	return Set_deleteGroup(self, fun, 2);
}


/*  change the size of a hash table	*/

imeth	gResize(int size)
{
	NODE	*v;
	int	i, sz;

	ENTERCRITICALSECTION(iCS);
	v = iTab;
	sz = iSize;
	iSize = size;
	iTab = Tncalloc(NODE, size);
	iNelm = 0;
	for (i=0 ; i != sz ; ++i)  {
		NODE	n, t;

		for (n=v[i] ; n ; n = t)  {
			Set_Lookup(self, n->luk, HT_ADD, 0, LTYPE, NULL);
			t = n->next;
			free_node(n);
		}
	}
	free(v);
	LEAVECRITICALSECTION(iCS);
	return self;
}

LOCAL	unsigned	Set_hash_string(char *s)
{
	register char	 c = 'a';
	double	t;
	register unsigned short	 k=0;  /* must be short	 */

	while (*s)
		k += *s++ ^ c++;
	t = .6125423371	* k;
	t = t < 0.0 ? -t : t;
	return (int) (BIG_INT * (t - floor(t)));
}

LOCAL	unsigned	Set_hash_short(int val)
{
	double	t;

	t = .6125423371	* (unsigned) val;
	t = t < 0.0 ? -t : t;
	return (unsigned) (BIG_INT * (t - floor(t)));
}

imeth	gLookup : Set_Lookup (luk, int mode, int deep, int type, value)
   	     	/*  0=Set, 1=Dictionary, 2=StringDictionary, 
		    3=ShortDictionary, 4=IntegerDictionary */
   	     	/*  0=no, 1=1 level, 2=deep	*/
{
	NODE	e, laste=NULL, newe;
	unsigned	 idx;
	int	i=0;
	struct _Object_iv_t * volatile ret;

	// Auto resize
	if ((double)iSize * .9 < (double)iNelm) {
		int	size = (int)((double)iSize * 1.6);
		gResize(self, size);
	}

	ENTERCRITICALSECTION(iCS);
	
	if (type == 2)
		idx = Set_hash_string((char *) luk) % iSize;
	else if (type == 3  ||  type == 4)
		idx = Set_hash_short(*((int *) luk)) % iSize;
	else
		idx = gHash(luk) % iSize;
	if (idx >= (unsigned) iSize)
		idx %= iSize;
	e = iTab[idx];
	while (1)  {
		if (e == NULL)	{
			if (mode != HT_ADD  &&  mode != HT_FINDADD) {
				LEAVECRITICALSECTION(iCS);
				return(NULL);
			}
			e = new_node();
			switch (type)  {
			case 0:
			default:
				e->luk = luk;
				break;
			case 1:
				e->luk = gNewWithObjObj(ObjectAssociation, luk, value);
				break;
			case 2:
				e->luk = gNewWithStrObj(StringAssociation, (char *) luk, value);
				break;
//			case 3:
//				e->luk = gNewWithIntObj(ShortAssociation, *((int *)luk), value);
//				break;
			case 4:
				e->luk = gNewWithIntObj(IntegerAssociation, *((int *)luk), value);
				break;
			}
			e->next	= NULL;
			if (laste)
				laste->next = e;
			else
				iTab[idx] = e;
			++iNelm;
			break;
		}
		switch (type)  {
		case 0:
			i = e->luk == luk ? 0 : gCompare(e->luk, luk);
			break;
		case 1:  {
			object	t = gKey(e->luk);
			i = t == luk ? 0 : gCompare(t, luk);
			break;
		}
		case 2:  {
			char	*t = gStringKey(e->luk);
			i = t == (char *) luk ? 0 : strcmp(t, (char *) luk);
			break;
		}
#if 0
		case 3: {
			int	c = gShortKey(e->luk);
			if (c == *((int *) luk))
				i = 0;
			else
				i = c < *((int *) luk) ? -1 : 1;
			break;
		  }
#endif
		case 4: {
			int	c = gIntKey(e->luk);
			if (c == *((int *) luk))
				i = 0;
			else
				i = c < *((int *) luk) ? -1 : 1;
			break;
		  }
		}
		if (!i)  {
			if (mode == HT_ADD) {
				LEAVECRITICALSECTION(iCS);
				return(NULL);
			}
			if (mode == HT_DELETE)  {
				if (deep == 1)
					gDispose(e->luk);
				else if (deep == 2)
					gDeepDispose(e->luk);
				if (laste)
					laste->next = e->next;
				else
					iTab[idx] = e->next;
				ret = e->luk;
				free_node(e);
				--iNelm;
				LEAVECRITICALSECTION(iCS);
				return deep ? self : (object) ret;
			}  
			break;
		}  else if (i > 0)  {
			if (mode != HT_ADD  &&  mode != HT_FINDADD) {
				LEAVECRITICALSECTION(iCS);
				return(NULL);
			}
			newe = new_node();
			switch (type)  {
			case 0:
			default:
				newe->luk = luk;
				break;
			case 1:
				newe->luk = gNewWithObjObj(ObjectAssociation, luk, value);
				break;
			case 2:
				newe->luk = gNewWithStrObj(StringAssociation, (char *) luk, value);
				break;
//			case 3:
//				newe->luk = gNewWithIntObj(ShortAssociation, *((int *) luk), value);
//				break;
			case 4:
				newe->luk = gNewWithIntObj(IntegerAssociation, *((int *) luk), value);
				break;
			}
			newe->next = e;
			if (laste)
				laste->next = newe;
			else
				iTab[idx] = newe;
			e = newe;
			++iNelm;
			break;
		}  else	 {
			laste =	e;
			e = e->next;
		}
	}
	ret = e->luk;
	LEAVECRITICALSECTION(iCS);
	return (object) ret;
}

imeth	gSequence ()
{
	return gNewSetSeq(SetSequence, iSize, iNelm, (void *) iTab);
}

static	void	class_init(void)
{
	INITIALIZECRITICALSECTION(cCS);
}





