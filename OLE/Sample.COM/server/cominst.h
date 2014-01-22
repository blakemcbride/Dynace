



/*
 *
 *	Copyright (c) 1996 Algorithms Corporation
 *	3020 Liberty Hills Drive
 *	Franklin, TN  37067
 *
 *	ALL RIGHTS RESERVED.
 *
 *
 *
 */



//  The following must match the ComInstance class

typedef	struct {
	IUnknown	iCI;
	int		iRef;
	IID		*iIID;
	object		iObj;
	object		iServer;
}	_UpperClass;

#define	GetIV(x)	ivType *iv = (ivType *)((char *)(x) + sizeof(_UpperClass))    //  a real kludge!






/*
 *
 *	Copyright (c) 1996 Algorithms Corporation
 *	3020 Liberty Hills Drive
 *	Franklin, TN  37067
 *
 *	ALL RIGHTS RESERVED.
 *
 *
 *
 */


