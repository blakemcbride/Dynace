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




#include <math.h>


defclass  Number;



static	double	Round(double n, int p)	/*  round n to p places	 */
{
	double	r;

	r = pow(10.0, (double) p);
	r = floor(.5 + fabs(n * r)) / r;
	if (n <	0.0)	return(-r);
	return(r);
}

static	int	Ndrgtb(double n, int b)	/* returns the number of digits	right of decimal point */
{
	double	base = b;
	int	i;

	n = n < 0.0 ? -n : n;
	i = 0;
	while (i < 20)	{
		n -= floor(n);
		if (1E-12 >= n)	 break;
		i++;
		n = Round(n*base, 13);
	}
	return(i);
}



/*	Numeric	formatter:
*	r = Nfmtb(n, b,	t, w, d, s);
*	double	n	number to be formatted
*	int	b	base
*	char *t		type of	format	- any combination of the following:
*			B = blank if zero
*			C = add	commas
*			L = left justify number
*			P = put	perenthises around negative numbers
*			Z = zero fill
*			D = floating dollar sign
*			U = uppercase letters in conversion
*			R = add a percent sign to the end of the number
*
*	int  w		total field width
*	ind  d		number of decimal places
*	char  s[w+1]	result of format
*	int  r		pointer	to result of format
*
*	example:
*
*		r = Nfmt(-12345.348, "CP", 12, 2, s);
*
*		result in s:	(12,345.35)
*/

static	char	*Nfmtb(double n, int b, char *t, int wth, int d, char *s)
{
	register int	si, i;
	int	sign, blnk, comma, left, paren,	zfill, nd, dol,	tw, dl,	ez, ucase, cf=3, w=wth, percent;
	double	base;
	static	char	alpha[]	= "0123456789abcdefghijklmnopqrstuvwxyz";

	if (b <	2  ||  b > (int)(sizeof(alpha)-1))
		b = 10;
	base = b;
	if (sign = n < 0.0)
		n = -n;

	/*  round number  */
	if (d >= 0)  {
		double	r = pow(base, (double) d);
//		n = floor(base/20.0 + n * r) / r;
		n = floor(.5 + n * r) / r;
	}
	
	switch (b)  {
		case 10:
			cf = 3;
			dl = n < 1.0 ? 0 : 1 + (int) log10(n);	/* # of	digits left of .  */
			break;
		case 2:
			cf = 4;
			dl = n < 1.0 ? 0 : 1 + (int) (log(n)/.6931471806);  /* # of digits left	of .  */
			break;
		case 8:
			cf = 3;
			dl = n < 1.0 ? 0 : 1 + (int) (log(n)/2.079441542);  /* # of digits left	of .  */
			break;
		case 16:
			cf = 4;
			dl = n < 1.0 ? 0 : 1 + (int) (log(n)/2.772588722);  /* # of digits left	of .  */
			break;
		default:
			cf = 3;
			dl = n < 1.0 ? 0 : 1 + (int) (log(n)/log(base));  /* # of digits left of .  */
			break;
	}
	if (d <	0)
		d = Ndrgtb(n, b);
	blnk = comma = left = paren = zfill = dol = ucase = percent = 0;
	if (t)
		while (*t)
			switch	(*t++)	{
			case  'B':	blnk  =	1;		break;
			case  'C':	comma =	(dl - 1) / cf;	break;
			case  'L':	left  =	1;		break;
			case  'P':	paren =	1;		break;
			case  'Z':	zfill =	1;		break;
			case  'D':	dol   =	1;		break;
			case  'U':	ucase =	1;		break;
			case  'R':	percent = 1;		break;
			}

	/*  calculate what the number should take up	*/

	ez = n < 1.0;
	tw = dol + paren + comma + sign	+ dl + d + !!d + ez + percent;

	if (w <	1)
		w = tw;
	else  if (tw > w)  {
		if (ez)
			tw -= ez--;
		if ((i=dol)  &&  tw > w)
			tw -= dol--;
		if (tw > w  &&	comma)	{
			tw -= comma;
			comma =	0;
		}
		if (tw < w  &&	i)  {
			tw++;
			dol = 1;
		}
		if (tw > w  &&	paren)
			tw -= paren--;
		if (tw > w && percent)
			tw -= percent--;
		if (tw > w)  {
nofit:
			for (i=0 ; i < w ; )
				s[i++] = '*';
			s[i] = '\0';
			return(s);
		}
	}

	n = floor(.5 + n * floor(.5 + pow(base,	(double) d)));
	if (blnk && n == 0.0)  {
		for (i=0 ; i < wth ; )
			s[i++] = ' ';
		s[wth] = '\0';
		return(s);
	}
	s[si = w] = '\0';
	if (left  &&  w	> tw)  {
		i = w -	tw;
		while (i--)
			s[--si]	= ' ';
	}
	if (paren)
		s[--si]	= sign ? ')' : ' ';
	if (percent)
		s[--si] = '%';
	for (nd=0 ; nd < d  &&	si ; nd++)  {
		n /= base;
		i = (int) floor(base * (n - floor(n)) + .5);
		n = floor(n);
		s[--si]	= ucase	&& i > 9 ? alpha[i]+('A'-'a') :	alpha[i];
	}
	if (d)
		if (si)
			s[--si]	= '.';
		else
			n = 1.0;
	if (ez	&&  si > sign +	dol)
		s[--si]	= '0';
	nd = 0;
	while (n > 0.0	&&  si)
		if (comma && nd	== cf)	{
			s[--si]	= ',';
			nd = 0;
		}  else	 {
			n /= base;
			i = (int) floor(base * (n - floor(n)) + .5);
			n = floor(n);
			s[--si]	= ucase	&& i > 9 ? alpha[i]+('A'-'a') :	alpha[i];
			nd++;
		}
	if (zfill)  {
		i = sign + dol;
		while (si > i)
			s[--si]	= '0';
	}
	if (dol	 &&  si)
		s[--si]	= '$';
	if (sign)
		if (si)
			s[--si]	= paren	? '(' :	'-';
		else
			n = 1.0;	/*  signal error condition	*/
	while (si)
		s[--si]	= ' ';
	if (n != 0.0)
		goto nofit;   /*  should never happen. but just	incase	*/
	return(s);
}

imeth	gFormatNumber, <vFormat> (char *msk, int wth, int dp)
{
	char	buf[80];

	Nfmtb(gDoubleValue(self), 10, msk, wth, dp, buf);
	return gNewWithStr(String, buf);
}

#if 0

//imeth	int	gHash()
{
	double	t;

	t = .6125423371	* gDoubleValue(self);
	t = t < 0.0 ? -t : t;
	return (int) (BIG_INT * (t - floor(t)));
}

#endif

imeth	int	gCompare(obj)
{
	double	sv, ov;

	ChkArg(obj, 2);
	if (!gIsKindOf(obj, Number))
		return gCompare(super, obj);
	if ((sv=gDoubleValue(self)) < (ov=gDoubleValue(obj)))
		return -1;
	else if (sv == ov)
		return 0;
	else
		return 1;
}




