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




#ifdef _WIN32
#include <pshpack2.h>
#endif

#ifndef	ALIGN2
#ifdef	__GNUC__
#define	ALIGN2	__attribute__ ((packed, aligned(2)))
#else
#define	ALIGN2
#endif
#endif


typedef	struct  _Head  {
	long	time  ALIGN2;		/*  time transaction was created	*/
	long	station  ALIGN2;	/*  station who originated transaction	*/
	long	user ALIGN2;		/*  user who made change		*/
	long	route  ALIGN2;		/*  station or (group) to get transaction */
	char	type;			/*  transaction type ACDE		*/
	unsigned char	version;	/*  version number			*/
	long	table ALIGN2;		/*  table id (string ID) or length of execute string	*/
}	Head;

typedef	struct 	_Field {
	long	field ALIGN2;		/*  field id (string ID)		*/
	char	type;			/*  data type Z=EOF			*/
	char	cval;			/*  Add/Delete - value if character	*/
					/*  Change - number of data fields  	*/
}	Field;

typedef	struct {
	char	sig[4]  ALIGN2;
	long	version  ALIGN2;
	long	time  ALIGN2;		/*  time files combined	*/
	long	tlen  ALIGN2;
	long	slen  ALIGN2;
}	CHead;



#ifdef _WIN32
#include <poppack.h>
#endif









