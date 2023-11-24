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

#define	OBJECTS_PER_NODE	50


#define	MEMMOVE(a,b,c)	memmove((void *)(a), (void *)(b), c)
#define	MEMSET(a,b,c)	memset((void *)(a), b, c)
#define	MEMCPY(a,b,c)	memcpy((void *)(a), (void *)(b), c)

defclass  BTreeNode {
	int	iUsed;				//  number of used iKeys
	int	iType;				//  1=intermediate, 2=leaf
	iKeys[OBJECTS_PER_NODE];
	iObjects[OBJECTS_PER_NODE+1];		//  data or intermediate nodes


	iBTree;					//  BTree instance
	iPrevious;				//  previous node (only used as temporary during processing)
                                        	//  creates a linked list up the chain
class:
	cData;					//  just some constant to be used as default data
};


cmeth	gNewNode(btree, int type)
{
	object	obj = gNew(super);
	accessIVsOf(obj);
	iBTree = btree;
	iType = type;
	if (!cData)
		cData = gNew(Constant);
	return obj;
}

imeth	gDispose()
{
	int	i;
	object	n;

	if (iType == 1) {
		for (i=0 ; i < iUsed ; ++i) {
			if (n = iKeys[i])
				gDeepDispose(n);
			if (n = iObjects[i])
				gDispose(n);
		}
		if (n = iObjects[iUsed])
			gDispose(n);
	}
	return gDispose(super);
}

imeth	gDeepDispose()
{
	int	i;
	object	n;

	for (i=0 ; i < iUsed ; ++i) {
		if (n = iKeys[i])
			gDeepDispose(n);
		if (n = iObjects[i])
			gDeepDispose(n);
	}
	if (iType == 1  &&  (n = iObjects[iUsed]))
		gDeepDispose(n);
	return gDispose(super);
}

static	int	bsearch2(ivType *iv, int (*cfun)(void *, void *), object key, int *idx)
{
	int	low = 0, high = iUsed-1, mid, cond;

	while (low <= high) {
		mid = (low + high) / 2;
		cond = cfun(key, iKeys[mid]);
		if (cond < 0)
			high = mid - 1;
		else if (cond > 0)
			low = mid + 1;
		else 
			break;
	}
	if (low <= high) {	//  found
		*idx = mid;
		return 1;
	} else {
		*idx = low;
		return 0;
	}
}

imeth	gFindBTNEQ : find (ifun cfun, key, object *foundKey)
{
	int	found, idx;

	found = bsearch2(iv, cfun, key, &idx);
	if (iType == 2)
		if (found) {
			if (foundKey)
				*foundKey = iKeys[idx];
			return iObjects[idx];
		} else
			return NULL;
	return find(iObjects[found+idx], cfun, key, foundKey);
}

imeth	gFindBTNFirst : findFirst (ifun cfun, object *foundKey)
{
	if (iType == 2) {
		if (foundKey)
			*foundKey = iKeys[0];
		return iObjects[0];
	}
	return findFirst(iObjects[0], cfun, foundKey);
}

imeth	gFindBTNLast : findLast (ifun cfun, object *foundKey)
{
	if (iType == 2) {
		if (foundKey)
			*foundKey = iKeys[iUsed-1];
		return iObjects[iUsed-1];
	}
	return findLast(iObjects[iUsed], cfun, foundKey);
}

imeth	gFindBTNGE : findGE (ifun cfun, object key, object *foundKey)
{
	int	found, idx;
	object	ret;

	found = bsearch2(iv, cfun, key, &idx);
	if (iType == 2)
		if (idx != iUsed) {
			if (foundKey)
				*foundKey = iKeys[idx];
			return iObjects[idx];
		} else {
			if (foundKey)
				*foundKey = NULL;
			return NULL;
		}
	for (idx=idx+found ; idx <= iUsed ; idx++)
		if (ret = findGE(iObjects[idx], cfun, key, foundKey))
			return ret;
	return NULL;
}

imeth	gFindBTNGT : findGT (ifun cfun, object key, object *foundKey)
{
	int	found, idx;
	object	ret;

	found = bsearch2(iv, cfun, key, &idx);
	if (found)
		idx++;
	if (iType == 2)
		if (idx != iUsed) {
			if (foundKey)
				*foundKey = iKeys[idx];
			return iObjects[idx];
		} else {
			if (foundKey)
				*foundKey = NULL;
			return NULL;
		}
	for ( ; idx <= iUsed ; idx++)
		if (ret = findGT(iObjects[idx], cfun, key, foundKey))
			return ret;
	return NULL;
}

imeth	gFindBTNLE : findLE (ifun cfun, object key, object *foundKey)
{
	int	found, idx;
	object	ret;

	found = bsearch2(iv, cfun, key, &idx);
	if (iType == 2)
		if (found) {
			if (foundKey)
				*foundKey = iKeys[idx];
			return iObjects[idx];
		} else if (idx) {
			if (foundKey)
				*foundKey = iKeys[idx-1];
			return iObjects[idx-1];
		} else {
			if (foundKey)
				*foundKey = NULL;
			return NULL;
		}
	for (idx=idx+found ; idx >= 0 ; idx--)
		if (ret = findLE(iObjects[idx], cfun, key, foundKey))
			return ret;
	return NULL;
}

imeth	gFindBTNLT : findLT (ifun cfun, object key, object *foundKey)
{
	int	idx;
	object	ret;

	bsearch2(iv, cfun, key, &idx);
	if (iType == 2)
		if (idx) {
			if (foundKey)
				*foundKey = iKeys[idx-1];
			return iObjects[idx-1];
		} else {
			if (foundKey)
				*foundKey = NULL;
			return NULL;
		}
	for ( ; idx >= 0 ; idx--)
		if (ret = findLT(iObjects[idx], cfun, key, foundKey))
			return ret;
	return NULL;
}

static	void	delete_intermediate_pointer(ivType *iv, object old_node)
{
	int	idx;
	
	for (idx=0 ; iObjects[idx] != old_node ; idx++);
	if (!idx) {
		gDeepDispose(iKeys[0]);
		MEMMOVE(iKeys, iKeys+1, (iUsed-1)*sizeof(object));
		MEMMOVE(iObjects, iObjects+1, iUsed*sizeof(object));
	} else {
		gDeepDispose(iKeys[idx-1]);
		MEMMOVE(iKeys+idx-1, iKeys+idx, (iUsed-idx)*sizeof(object));
		MEMMOVE(iObjects+idx, iObjects+idx+1, (iUsed-idx)*sizeof(object));
	}
	iKeys[iUsed-1] = iObjects[iUsed] = NULL;
	iUsed--;
}

static	void	collapse(ivType *liv, object lo, int deep, object save_pointer)
{
	object	self = liv->iPrevious;
	ivType	*iv = ivPtr(self);

	if (liv->iType == 2) {
		if (deep  ||  liv->iType == 1)
			gDeepDispose(lo);
		else
			gDispose(lo);
		if (!iPrevious)                                              //  Top node & Leaf node
			if (iUsed == 1) {
				gSetTopNode(iBTree, iObjects[iObjects[0] == lo]);
				iObjects[0] = iObjects[1] = NULL;
				gDeepDispose(self);
			} else
				delete_intermediate_pointer(iv, lo);
		else                                                        //  Intermediate node & Leaf node
			if (iUsed == 1) {
				object save = iObjects[0] == lo ? iObjects[1] : iObjects[0];
				iObjects[0] = iObjects[1] = NULL;  //  Don't delete these objects when self is disposed
				collapse(iv, self, deep, save);
			} else
				delete_intermediate_pointer(iv, lo);
	} else {                                            //  Intermediate (or Top) & another intermediate node
		int	idx;

		for (idx=0 ; iObjects[idx] != lo ; idx++);
		iObjects[idx] = save_pointer;
		gDeepDispose(lo);
	}
}

imeth	gDeleteBTNode : delete (ifun cfun, key, int deep, prev)
{
	int	found, idx;
	object	res;

	found = bsearch2(iv, cfun, key, &idx);
	if (iType == 2) {
		if (!found)
			return NULL;
		if (iUsed == 1  &&  prev) {
			iPrevious = prev;
			collapse(iv, self, deep, NULL);
		} else {
			int	n = iUsed - idx - 1;
			if (deep) {
				gDeepDispose(iKeys[idx]);
				gDeepDispose(iObjects[idx]);
			}
			MEMMOVE(iKeys+idx, iKeys+idx+1, n*sizeof(object));
			MEMMOVE(iObjects+idx, iObjects+idx+1, n*sizeof(object));
			iUsed--;
			iKeys[iUsed] = iObjects[iUsed] = NULL;
		}
		return self;
	}
	iPrevious = prev;
	res = delete(iObjects[found+idx], cfun, key, deep, self);
	iPrevious = NULL;
	return res;
}

static	object	split(object lo, ivType *left, ifun cfun)
{
	object	to, ro, ret;
 	ivType	*right, *top;
	int	lhalf, rhalf;

	ro = gNewNode(CLASS, left->iBTree, left->iType);
 	right = ivPtr(ro);
	if (left->iType == 2) {
		lhalf = left->iUsed / 2;
		rhalf = left->iUsed - lhalf;
		MEMCPY(right->iKeys, left->iKeys+lhalf, rhalf*sizeof(object));
		MEMSET(left->iKeys+lhalf, 0, rhalf*sizeof(object));
		left->iUsed = lhalf;
		right->iUsed = rhalf;
		MEMCPY(right->iObjects, left->iObjects+lhalf, rhalf*sizeof(object));
		MEMSET(left->iObjects+lhalf, 0, rhalf*sizeof(object));
	} else {
		lhalf = left->iUsed / 2;
		rhalf = left->iUsed - lhalf - 1;
		MEMCPY(right->iKeys, left->iKeys+lhalf+1, rhalf*sizeof(object));
		MEMSET(left->iKeys+lhalf+1, 0, rhalf*sizeof(object));
		left->iUsed = lhalf;
		right->iUsed = rhalf;
		MEMCPY(right->iObjects, left->iObjects+lhalf+1, (rhalf+1)*sizeof(object));
		MEMSET(left->iObjects+lhalf+1, 0, (rhalf+1)*sizeof(object));
	}

	if (!(ret=to=left->iPrevious)) {
		ret = to = gNewNode(CLASS, left->iBTree, 1);
		gSetTopNode(left->iBTree, to);
		top = ivPtr(to);
		if (left->iType == 1) {
			top->iKeys[0] = left->iKeys[lhalf];
			left->iKeys[lhalf] = NULL;
		} else
			top->iKeys[0] = gDeepCopy(right->iKeys[0]);
		top->iObjects[0] = lo;
		top->iObjects[1] = ro;
		top->iUsed = 1;
	} else {
		int	found, idx, n;

		top = ivPtr(to);
		if (top->iUsed == OBJECTS_PER_NODE) {
			ret = to = split(to, top, cfun);
			top = ivPtr(to);
			found = bsearch2(top, cfun, right->iKeys[0], &idx);
			to = top->iObjects[found+idx];
			top = ivPtr(to);
		}
		found = bsearch2(top, cfun, right->iKeys[0], &idx);
		if (found)
			gError(Dynace, "BTreeNode error");
		n = top->iUsed - idx;
		if (n) {
			MEMMOVE(top->iKeys+idx+1, top->iKeys+idx, n*sizeof(object));
			MEMMOVE(top->iObjects+idx+2, top->iObjects+idx+1, n*sizeof(object));
		}
		if (left->iType == 1) {
			top->iKeys[idx] = left->iKeys[lhalf];
			left->iKeys[lhalf] = NULL;
		} else
			top->iKeys[idx] = gDeepCopy(right->iKeys[0]);
		top->iObjects[idx+1] = ro;
		top->iUsed++;
	}

	return ret;
}

/*
  cfun		key comparison function (args like strcmp)
  key		key used for comparisons
  data		data to be associated with key
  replace	if key already found 0=don't replace, 1=replace
  replaced	0=dup found and not replaced, 1=new key added, 2=dup key found and data replaced - key not used
  prev		previous node in search path
  old		old data (if dup key found and data replaced)
*/

#define	DATA	data ? data : cData

imeth	gAddBTreeNode : add (ifun cfun, key, data, int replace, int *replaced, prev, object *old)
{
	int	found, idx;
	object	tmp,  ret;

	found = bsearch2(iv, cfun, key, &idx);
	if (iType == 1) {
		iPrevious = prev;
		ret = add(iObjects[found+idx], cfun, key, DATA, replace, replaced, self, old);
		iPrevious = NULL;
		return ret;
	}

	if (found)			//  already exists
		if (replace) {
			*old = iObjects[idx];
			iObjects[idx] = DATA;
			*replaced = 2;
			return self;
		} else {
			*replaced = 0;
			return self;
		}

	//  node doesn't already exist - insert

	if (iUsed == OBJECTS_PER_NODE) {	//  node full
		iPrevious = prev;
		tmp = split(self, iv, cfun);
		iPrevious = NULL;
		return add(tmp, cfun, key, DATA, replace, replaced, NULL, old);
	}

	if (idx != iUsed) {     		//  not append
		int	n = iUsed - idx;
		MEMMOVE(iKeys+idx+1, iKeys+idx, n*sizeof(object));
		MEMMOVE(iObjects+idx+1, iObjects+idx, n*sizeof(object));
	}
	iKeys[idx] = key;
	iObjects[idx] = DATA;
	iUsed++;
	*replaced = 1;
	return self;
}

imeth	gPrint(stream)
{
	int	i, n;

	gPrintValue(self, stream);
	gPuts(stream, "\n------------------------------------------------------------\n");
	if (iType == 1) {
		n = iUsed + 1;
		for (i=0 ; i < n ; i++)
			gPrint(iObjects[i], stream);
	}
	return self;
}

imeth	gPrintValue(stream)
{
	int	i, n;
	object	t, k;
	
	vPrintf(stream, "BTree %8.8lx, %s node %8.8lx, %d used\n\n", iBTree, iType == 1 ? "Intermediate" : "Leaf", self, iUsed);
	if (!iBTree)
		vPrintf(stream, "ERROR: iBTRee not set\n");
	if (iPrevious)
		vPrintf(stream, "ERROR: iPrevious set\n");
	for (i=iUsed ; i < OBJECTS_PER_NODE ; i++)
		if (iKeys[i])
			vPrintf(stream, "\nERROR:  iKeys[%d] has an unexpected value.\n", i);
	for (i=iUsed+(iType==1) ; i <= OBJECTS_PER_NODE ; i++)
		if (iObjects[i])
			vPrintf(stream, "\nERROR:  iObjects[%d] has an unexpected value.\n", i);
	if (iType == 1) {
		n = iUsed + 1;
		for (i=0 ; i < n ; i++) {
			vPrintf(stream, "\t%8.8lx  \n", iObjects[i]);
			if (i < iUsed) {
				k = gStringRepValue(iKeys[i]);
				vPrintf(stream, "\t\t%s  \n", gStringValue(k));
				gDispose(k);
			}
		}
	} else
		for (i=0 ; i < iUsed ; i++) {
			t = gStringRepValue(iObjects[i]);
			k = gStringRepValue(iKeys[i]);
			vPrintf(stream, "\t%s - %s \n", gStringValue(k), gStringValue(t));
			gDispose(k);
			gDispose(t);
		}
	gPuts(stream, "\n");
	return self;
}

