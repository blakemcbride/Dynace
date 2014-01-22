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




#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#endif
#endif



#include <string.h>
#include "memalloc.h"


#include "array1.h"
#include "array2.h"



defclass  Array  {
	char		iType;
	unsigned	iRank;
	INDEX_TYPE	*iShape;
	INDEX_TYPE	iNelm;
	void		*iArray;
	void		*iRmp;		/*  registered memory pointer  */
};

#ifdef	PROFILE
#undef	PMETHOD
#define	PMETHOD
#endif


static	int	Index_origin = 0;

static	int _A_esize(int type);
static	void print_val(object str,ivType *iv,char *fmt1,char *fmt2);
static	void p_val_mat(object str,unsigned rank,unsigned *shape,char **val ,int size,char *fmt,unsigned *bit_indx,char *buf, int typ);
static	objrtn print_nest(object s, ivType *iv);
static	objrtn Array_Dup(object self,int ntype,int dval,int deep);
static	int convert(ivType *iv,ivType *iv2);



static	unsigned char	pow1[] = { 1, 2, 4, 8, 16, 32, 64, 128 };
static	unsigned char	pow2[] = { ~1, ~2, ~4, ~8, ~16, ~32, ~64,
			   (unsigned char)~128 };

static	char	OOB[] = "Error: Out of bounds array index.\n";


cmeth	gNew()
{
	return gShouldNotImplement(self, "gNew");
}

cmeth	gNewArray(int type, int rank, va_list _rest_)  /*  same as the following method */
{
	int	i;
	INDEX_TYPE	n;
	object	array = gNew(super);
	ivType	*iv = ivPtr(array);

	iType = type;
	iRank = rank;
	iShape = rank ? MTnalloc(INDEX_TYPE, rank, iShape) : (INDEX_TYPE *) NULL;
	for (i=0, n=1 ; i < rank ; ++i)
		n *= iShape[i] = GetArg(unsigned);
	iNelm = n;
	if (type == AT_OBJ)  {
		iArray = n ? Tncalloc(char, (unsigned) SIZE(type, n)) : NULL;
		iRmp = gRegisterMemory(Dynace, iArray, (long) SIZE(type, n));
	}  else
		iArray = n ? MTncalloc(char, (unsigned) SIZE(type, n), iArray) : NULL;
	return array;
}

/*  same as the previous method  */

private	cmeth	Array_NewArray(int type, int rank, unsigned *idx)
{
	int	i;
	INDEX_TYPE	n;
	object	array = gNew(super);
	ivType	*iv = ivPtr(array);

	iType = type;
	iRank = rank;
	iShape = rank ? MTnalloc(INDEX_TYPE, rank, iShape) : (INDEX_TYPE *) NULL;
	for (i=0, n=1 ; i < rank ; ++i)
		n *= iShape[i] = idx[i];
	iNelm = n;
	if (type == AT_OBJ)  {
		iArray = n ? Tncalloc(char, (unsigned) SIZE(type, n)) : NULL;
		iRmp = gRegisterMemory(Dynace, iArray, (long) SIZE(type, n));
	}  else
		iArray = n ? MTncalloc(char, (unsigned) SIZE(type, n), iArray) : NULL;
	return array;
}

imeth	object	gDispose, gGCDispose ()
{
	if (iShape)
		MA_free(iShape);
	if (iArray)
		if (iType == AT_OBJ)
			free(iArray);
		else
			MA_free(iArray);
	if (iRmp)
		gRemoveRegisteredMemory(Dynace, iRmp);
	gDispose(super);
	return NULL;
}

imeth	object	gDeepDispose()
{
	object	*v;
	INDEX_TYPE	i;

	if (iType == AT_OBJ)  {
		v = (object *) iArray;
		for (i=0 ; i != iNelm ; ++i)
			if (v[i])  {
				gDeepDispose(v[i]);
				v[i] = NULL;
			}
	}

	if (iShape)
		MA_free(iShape);
	if (iArray)
		if (iType == AT_OBJ)
			free(iArray);
		else
			MA_free(iArray);
	if (iRmp)
		gRemoveRegisteredMemory(Dynace, iRmp);
	gDispose(super);
	return NULL;
}

imeth	void	*gArrayPointer()
{
	return iArray;
}

imeth	unsigned  gRank()
{
	return iRank;
}

imeth	gShape()
{
	unsigned  i;
	object	r;
	ivType	*iv2;

	r = vNew(ShortArray, 1, iRank);
	iv2 = ivPtr(r);

	for (i=0 ; i < iRank ; ++i)
		((INDEX_TYPE *) iv2->iArray)[i] = iShape[i];

	return r;
}

imeth	void	*gIndex(va_list _rest_)
{
	INDEX_TYPE	offset, r, i;

	if (iType == AT_BIT)
		gError(self, "Cannot use gIndex on BitArray\n");
	if (!iRank)
		return iArray;	/* scalor	*/
	r = iRank - 1;
	for (i=0, offset=(INDEX_TYPE) 0 ; i <= r ; ++i)  {
		INDEX_TYPE	m, n, j;
		
		n = GetArg(unsigned) - Index_origin;
		if (n >= iShape[i])		/*  Index out of bounds  */
			gError(self, OOB);
		for (m=1, j=r ; j != i ; )
			m *= iShape[j--];
		if (!m)		/* 0 Diminsion size  	*/
			gError(self, OOB);
		offset += m * n;
	}
	return (void *) ((char *) iArray + _A_esize(iType) * offset);
}

ivmeth	int	vBitValue(...)
{
	int		i, r;
	INDEX_TYPE	offset;
	MAKE_REST(self);

	if (iType != AT_BIT)
		gError(self, "Error: Can't use vBitValue on non-BitArrays\n");
	r = iRank - 1;
	for (i=0, offset=(INDEX_TYPE) 0 ; i <= r ; ++i)  {
		INDEX_TYPE	m;
		INDEX_TYPE	n;
		register int	j;
		
		n = GetArg(INDEX_TYPE) - Index_origin;
		if (n >= iShape[i])	/*  Index out of bounds  */
			gError(self, OOB);
		for (m=1, j=r ; j != i ; )
			m *= iShape[j--];
		if (!m)		/* 0 Diminsion size  	*/
			gError(self, OOB);
		offset += m * n;
	}
	return !!BIT_VAL(iArray, offset);
}

ivmeth	vChangeBitValue(int v, ...)
{
	int		i, r;
	INDEX_TYPE	offset;
	MAKE_REST(v);

	if (iType != AT_BIT)
		gError(self, "Error: Can't use vChangeBitValue on non-BitArrays\n");
	r = iRank - 1;
	for (i=0, offset=(INDEX_TYPE) 0 ; i <= r ; ++i)  {
		INDEX_TYPE	m;
		INDEX_TYPE	n;
		register int	j;
		
		n = GetArg(INDEX_TYPE) - Index_origin;
		if (n >= iShape[i])		/*  Index out of bounds  */
			gError(self, OOB);
		for (m=1, j=r ; j != i ; )
			m *= iShape[j--];
		if (!m)		/* 0 Diminsion size  	*/
			gError(self, OOB);
		offset += m * n;
	}
	SET_BIT(iArray, offset, v);
	return self;
}

cmeth	gIota(int n)
{
	INDEX_TYPE	c;
	int	i;
	object	a;
	ivType	*iv;
	
	USE(self);
	a = vNew(ShortArray, 1, n);
	iv = ivPtr(a);
	for (c=Index_origin, i=0 ; i < n ; )
		((short *) iArray)[i++] = c++;
	return a;
}

ivmeth	object	vReshape(unsigned rank, ...)
{
	INDEX_TYPE	n, *shape, d, i;
	MAKE_REST(rank);

	shape = rank ? MTnalloc(INDEX_TYPE, rank, iShape) : (INDEX_TYPE *) NULL;
	for (i=0, n=1 ; i < rank ; ++i)  {
		d = GetArg(INDEX_TYPE);
		n *= d;
		shape[i] = d;
	}

	if (iNelm != n)  {
		char		*fp, *tp;
		void		*array;
		INDEX_TYPE	s1, s2, s1org;

		s1org = s1 = SIZE(iType, iNelm);
		s2 = SIZE(iType, n);
		if (iType == AT_OBJ)
			array = n ? Tncalloc(char, s2) : NULL;
		else
			array = n ? MTncalloc(char, s2, iArray) : NULL;

		fp = (char *) iArray;
		tp = (char *) array;
		while (s2)  {
			unsigned	m;

			m = s2 < s1 ? s2 : s1;
			memcpy(tp, fp, m);
			s2 -= m;
			s1 -= m;
			if (!s1)  {
				fp = (char *) iArray;
				s1 = s1org;
			} else
				fp += m;
			tp += m;
		}
		if (iArray)
			if (iType == AT_OBJ)
				free(iArray);
			else
				MA_free(iArray);
		iArray = array;

		iNelm = n;
		if (iType == AT_OBJ)  {
			void	*rmp = gRegisterMemory(Dynace, iArray, (long) SIZE(iType, iNelm));
			gRemoveRegisteredMemory(Dynace, iRmp);
			iRmp = rmp;
		}
	}
	iRank = rank;
	if (iShape)
		MA_free(iShape);
	iShape = shape;
	
	return self;
}

/* returns the size of individual elements  */

static	int	_A_esize(int type)
{
	switch (type)  {
		case AT_CHAR:		return sizeof(char);
		case AT_SHRT:		return sizeof(short);
		case AT_USHT:		return sizeof(_ushort);
		case AT_INT:		return sizeof(int);
		case AT_LONG:		return sizeof(long);
		case AT_FLOT:		return sizeof(float);
		case AT_DBLE:		return sizeof(double);
		case AT_OBJ:		return sizeof(object);
		case AT_BIT:		return 0;
		case AT_PNTR:		return sizeof(char *);
		default:		return 0;
	}
}

imeth	int	gSize()
{
	return iNelm;
}

imeth	int	gEqual(obj)
{
	ivType	*iv2;
	unsigned	i;
	
	ChkArg(obj, 2);
	if (!gIsKindOf(obj, CLASS))
		return 0;
	iv2 = ivPtr(obj);
	if (iType != iv2->iType  ||  iRank != iv2->iRank  ||  iNelm != iv2->iNelm)
		return 0;
	for (i=0 ; i < iRank ; ++i)
		if (iShape[i] != iv2->iShape[i])
			return 0;
	return iArray ? !memcmp(iArray, iv2->iArray, (int) SIZE(iType, iNelm)) : 1;
}

imeth	gStringRepValue()
{
	object	s;

	s = gNew(String);
	switch (iType)  {
		case AT_CHAR:	print_val(s, iv, "%c", "%c");		break;
		case AT_SHRT:	print_val(s, iv, "%hd ", "%6hd ");	break;
		case AT_USHT:	print_val(s, iv, "%hu ", "%5hu ");	break;
		case AT_INT:	print_val(s, iv, "%d ", "%6d ");	break;
		case AT_LONG:	print_val(s, iv, "%ld ", "%10ld ");	break;
		case AT_FLOT:	print_val(s, iv, "%hf ", "%10.2hf ");	break;
		case AT_DBLE:	print_val(s, iv, "%lf ", "%10.2lf ");	break;
		case AT_OBJ:	print_nest(s, iv);			break;
		case AT_BIT:	print_val(s, iv, "%d ", "%1d ");	break;
		case AT_PNTR:	print_val(s, iv, "%lx ", "%8lx ");	break;
	}
	return s;
}

imeth	gStringRep()
{
	char	*t, buf[60];
	object	s;

	switch (iType)  {
		case AT_CHAR:	t = "Character";	break;
		case AT_SHRT:	t = "Short";		break;
		case AT_USHT:	t = "Unsigned Short";	break;
		case AT_INT:	t = "Integer";		break;
		case AT_LONG:	t = "Long";		break;
		case AT_FLOT:	t = "Float";		break;
		case AT_DBLE:	t = "Double";		break;
		case AT_OBJ:	t = "Object Array";	break;
		case AT_BIT:	t = "Bit";		break;
		case AT_PNTR:	t = "Pointer";		break;
		default:	t = "Unknown";		break;
	}
	s = vSprintf(String, "Type  = %s\n", t);
	sprintf(buf, "Rank  = %d\n", (int) iRank);
	gAppend(s, (object) buf);
	
	if (iRank)  {
		unsigned	i;

		gAppend(s, (object) "Shape = ");
		for (i=0 ; i < iRank ; )  {
			sprintf(buf, PRNT_SHAPE, iShape[i++]);
			gAppend(s, (object) buf);
		}
		gAppend(s, (object) "\n");
	}
	gAppend(s, (object) "Value = ");
	switch (iType)  {
		case AT_CHAR:	print_val(s, iv, "%c", "%c");		break;
		case AT_SHRT:	print_val(s, iv, "%hd ", "%6hd ");	break;
		case AT_USHT:	print_val(s, iv, "%hu ", "%5hu ");	break;
		case AT_INT:	print_val(s, iv, "%d ", "%6d ");	break;
		case AT_LONG:	print_val(s, iv, "%ld ", "%10ld ");	break;
		case AT_FLOT:	print_val(s, iv, "%hf ", "%10.2hf ");	break;
		case AT_DBLE:	print_val(s, iv, "%lf ", "%10.2lf ");	break;
		case AT_OBJ:	print_nest(s, iv);			break;
		case AT_BIT:	print_val(s, iv, "%d ", "%1d ");	break;
		case AT_PNTR:	print_val(s, iv, "%lx ", "%8lx ");	break;
	}
	return s;
}

static	void	_fmt(char *buf, char *fmt, void *var, int typ)
{
	switch (typ)  {
	case AT_CHAR:	sprintf(buf, fmt, *((char *) var));	break;
	case AT_SHRT:	sprintf(buf, fmt, *((short *) var));	break;
	case AT_USHT:	sprintf(buf, fmt, *((_ushort *) var));	break;
	case AT_INT:	sprintf(buf, fmt, *((int *) var));	break;
	case AT_LONG:	sprintf(buf, fmt, *((long *) var));	break;
	case AT_FLOT:	sprintf(buf, fmt, *((float *) var));	break;
	case AT_DBLE:	sprintf(buf, fmt, *((double *) var));	break;
	case AT_OBJ:	sprintf(buf, fmt, *((object *) var));	break;
	case AT_PNTR:	sprintf(buf, fmt, *((void **) var));	break;
	}
}

static	void	print_val(object str, ivType *iv, char *fmt1, char *fmt2)
{
	INDEX_TYPE	i, bit_indx = 0;
	int	s = _A_esize(iType);
	char	buf[60], *val;
	
	switch (iRank)  {
		case 0:	if (iType != AT_BIT)  {
				_fmt(buf, fmt1, iArray, iType);
				if (iType == AT_CHAR)
					vBuild(str, NULL, "\"", buf, "\"\n", NULL);
				else
					vBuild(str, NULL, buf, "\n", NULL);
			}  else  {
				sprintf(buf, fmt1, !!BIT_VAL(iArray, 0));
				vBuild(str, NULL, buf, "\n", NULL);
			}
			break;
		case 1:	if (iType != AT_BIT)  {
				if (iType == AT_CHAR)
					gAppend(str, (object) "\"");
				val = (char *) iArray;
				for (i=0 ; i++ != *iShape ; val+=s)  {
					_fmt(buf, fmt1, val, iType);
					gAppend(str, (object) buf);
				}
				if (iType == AT_CHAR)
					gAppend(str, (object) "\"");
			}  else
				for (i=0 ; i != *iShape ; ++i)  {
					sprintf(buf, fmt1, !!BIT_VAL(iArray, i));
					gAppend(str, (object) buf);
				}
			gAppend(str, (object) "\n");
			break;
		default:gAppend(str, (object) "\n\n");
			val = (char *) iArray;
			p_val_mat(str, iRank, iShape, &val, s, fmt2, &bit_indx, buf, iType);
	}
}

static	void	p_val_mat(object str, unsigned rank, INDEX_TYPE *shape, char **val, int size, char *fmt, INDEX_TYPE *bit_indx, char *buf, int type)
{
	INDEX_TYPE	r, c;

	if (rank == 2)
		if (size)	/*  not a bit field  */
			for (r=0 ; r++ != *shape ; )  {
				for (c=0 ; c++ != shape[1] ; (*val)+=size)  {
					_fmt(buf, fmt, *val, type);
					gAppend(str, (object) buf);
				}
				gAppend(str, (object) "\n");
			}
		else
			for (r=0 ; r++ != *shape ; )  {
				for (c=0 ; c++ != shape[1] ; (*bit_indx)++)  {
					sprintf(buf, fmt, !!BIT_VAL(*val, *bit_indx));
					gAppend(str, (object) buf);
				}
				gAppend(str, (object) "\n");
			}
	else  
		for (r=0 ; r++ != *shape ; )  {
			p_val_mat(str, rank-1, shape+1, val, size, fmt, bit_indx, buf, type);
			gAppend(str, (object) "\n");
		}
}

static	objrtn	print_nest(object s, ivType *iv)
{
	INDEX_TYPE	i;
	object	*val = (object *) iArray;
	object	t;

	for (i=0 ; i++ != iNelm ; )  {
		if (t = *val++)  {
			t = gStringRepValue(*val++);
			vBuild(s, NULL, "\n", t, "\n", NULL);
			gDispose(t);
		} else
			gAppend(s, (object) "NULL\n");
	}
	return s;
}

/*  duplicate an array and optionally change the type	*/

private	imeth	Array_Dup(object self, int ntype, int dval, int deep)
      	     		/*  array to be duplicated	*/
   	      		/*  new array data type		*/
   	     		/*  duplicate value flag	*/
{
	register unsigned	i;
	object	narray, cls;
	ivType	*iv2;
	
	if (!ntype)
		ntype = iType;
	if (ntype != iType  &&  (
				ntype == AT_OBJ  ||  ntype == AT_PNTR  ||
				iType == AT_OBJ  ||   iType == AT_PNTR))
		gError(self, "Error:  Can't convert array to requested type.\n");

	switch (ntype)  {
	case AT_CHAR:	cls = CharacterArray;		break;
	case AT_SHRT:	cls = ShortArray;		break;
	case AT_USHT:	cls = UnsignedShortArray;	break;
	case AT_INT:	cls = IntegerArray;		break;
	case AT_LONG:	cls = LongArray;		break;
	case AT_FLOT:	cls = FloatArray;		break;
	case AT_DBLE:	cls = DoubleFloatArray;		break;
	case AT_BIT:	cls = BitArray;			break;
	case AT_OBJ:	cls = ObjectArray;		break;
	case AT_PNTR:	cls = PointerArray;		break;
	default:	cls = NULL;			break;
	}
	narray = Array_NewArray(cls, ntype, iRank, iShape);
	iv2 = ivPtr(narray);

	if (dval)
		if (ntype == iType)
			if (iType == AT_OBJ  &&  deep)  {
				object	*fv = (object *) iArray;
				object	*tv = (object *) iv2->iArray;
				for (i=0 ; i != iNelm ; ++i)
					if (fv[i])
						tv[i] = gDeepCopy(fv[i]);
			}  else
				memcpy(iv2->iArray, iArray, (int) SIZE(iType, iNelm));
		else
			convert(iv, iv2);
	return narray;
}

#define Ftod(x)	(double)(x)
#define Dtol(x)	(long)(x)

#if 0
#define CONV(tt, ft)	while (n--) *((tt *) nval)++ = (tt) *((ft *) val)++
#define CONVFI(tt, ft)	while (n--) *((tt *) nval)++ = (tt) Dtol((double)*((ft *) val)++)
#define CONVFD()	while (n--) *((double *) nval)++ = Ftod(*((float *) val)++)
#define CONVFB(tt)	while (n--) *((tt *) nval)++ = (tt) !!BIT_VAL(val, n)
#define CONVTB(ft)   for (m=0 ; m != n ; m++) SET_BIT(nval, m, *((ft *) val)++)
#else
#define CONV(tt, ft)		\
	while (n--)  {		\
	        *((tt *) nval) = (tt) *((ft *) val);			\
		nval = (void *) (1 + (tt *) nval);			\
		val  = (void *) (1 + (ft *) val);			\
	}
#define CONVFI(tt, ft)		\
	while (n--)  {		\
		*((tt *) nval) = (tt) Dtol((double)*(ft *) val);	\
		nval = (void *) (1 + (tt *) nval);			\
		val  = (void *) (1 + (ft *) val);			\
	}
#define CONVFD()		\
	while (n--)  {		\
		*((double *) nval) = Ftod(*((float *) val));		\
		nval = (void *) (1 + (double *) nval);			\
		val  = (void *) (1 + (float *) val);			\
	}
#define CONVFB(tt)		\
	while (n--)  {		\
		*((tt *) nval) = (tt) !!BIT_VAL(val, n);		\
		nval = (void *) (1 + (tt *) nval);			\
	}
#define CONVTB(ft)   		\
	for (m=0 ; m != n ; m++)  {					\
		SET_BIT(nval, m, *((ft *) val));			\
		val  = (void *) (1 + (ft *) val);			\
	}
#endif

static	int	convert(ivType *iv, ivType *iv2)
{
	INDEX_TYPE	m;
	INDEX_TYPE	n = iNelm;
	void	*val = iArray;
	void	*nval = iv2->iArray;

	switch (iv2->iType)  {
	case AT_CHAR:
		switch (iType)  {
		case AT_SHRT:	CONV(char, short);		break;
		case AT_USHT:	CONV(char, _ushort);		break;
		case AT_INT:	CONV(char, int);		break;
		case AT_LONG:	CONV(char, long);		break;
		case AT_FLOT:	CONVFI(char, float);		break;
		case AT_DBLE:	CONVFI(char, double);		break;
		case AT_BIT:	CONVFB(char);			break;
		default:	return(1);			break;
		}
		break;
	case AT_SHRT:
		switch (iType)  {
		case AT_CHAR:	CONV(short, char);		break;
		case AT_USHT:	CONV(short, _ushort);		break;
		case AT_INT:	CONV(short, int);		break;
		case AT_LONG:	CONV(short, long);		break;
		case AT_FLOT:	CONVFI(short, float);		break;
		case AT_DBLE:	CONVFI(short, double);		break;
		case AT_BIT:	CONVFB(short);			break;
		default:	return(1);			break;
		}
		break;
	case AT_USHT:
		switch (iType)  {
		case AT_CHAR:	CONV(_ushort, char);		break;
		case AT_SHRT:	CONV(_ushort, short);		break;
		case AT_INT:	CONV(_ushort, int);		break;
		case AT_LONG:	CONV(_ushort, long);		break;
		case AT_FLOT:	CONVFI(_ushort, float);		break;
		case AT_DBLE:	CONVFI(_ushort, double);	break;
		case AT_BIT:	CONVFB(_ushort);		break;
		default:	return(1);			break;
		}
		break;
	case AT_INT:
		switch (iType)  {
		case AT_CHAR:	CONV(int, char);		break;
		case AT_SHRT:	CONV(int, short);		break;
		case AT_USHT:	CONV(int, _ushort);		break;
		case AT_LONG:	CONV(int, long);		break;
		case AT_FLOT:	CONVFI(int, float);		break;
		case AT_DBLE:	CONVFI(int, double);		break;
		case AT_BIT:	CONVFB(int);			break;
		default:	return(1);			break;
		}
		break;
	case AT_LONG:
		switch (iType)  {
		case AT_CHAR:	CONV(long, char);		break;
		case AT_SHRT:	CONV(long, short);		break;
		case AT_USHT:	CONV(long, _ushort);		break;
		case AT_INT:	CONV(long, int);		break;
		case AT_FLOT:	CONVFI(long, float);		break;
		case AT_DBLE:	CONVFI(long, double);		break;
		case AT_BIT:	CONVFB(long);			break;
		default:	return(1);			break;
		}
		break;
	case AT_FLOT:
		switch (iType)  {
		case AT_CHAR:	CONV(float, char);		break;
		case AT_SHRT:	CONV(float, short);		break;
		case AT_USHT:	CONV(float, _ushort);		break;
		case AT_INT:	CONV(float, int);		break;
		case AT_LONG:	CONV(float, long);		break;
		case AT_DBLE:	CONV(float, double);		break;
		case AT_BIT:	CONVFB(float);			break;
		default:	return(1);			break;
		}
		break;
	case AT_DBLE:
		switch (iType)  {
		case AT_CHAR:	CONV(double, char);		break;
		case AT_SHRT:	CONV(double, short);		break;
		case AT_USHT:	CONV(double, _ushort);		break;
		case AT_INT:	CONV(double, int);		break;
		case AT_LONG:	CONV(double, long);		break;
		case AT_FLOT:	CONVFD();			break;
		case AT_BIT:	CONVFB(double);			break;
		default:	return(1);			break;
		}
		break;
	case AT_BIT:
		switch (iType)  {
		case AT_CHAR:	CONVTB(char);			break;
		case AT_SHRT:	CONVTB(short);			break;
		case AT_USHT:	CONVTB(_ushort);		break;
		case AT_INT:	CONVTB(int);			break;
		case AT_LONG:	CONVTB(long);			break;
		case AT_FLOT:	CONVTB(float);			break;
		case AT_DBLE:	CONVTB(double);			break;
		default:	return(1);			break;
		}
		break;
	default:	return(1);		break;
	}
	return(0);
}

imeth	gCopy()
{
	return Array_Dup(self, 0, 1, 0);
}

imeth	gDeepCopy()
{
	return Array_Dup(self, 0, 1, 1);
}

cmeth	gIndexOrigin(int n)
{
	Index_origin = n;
	return self;
}




