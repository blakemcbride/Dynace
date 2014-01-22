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



#ifndef	__COSMIC__
#include <time.h>
#endif
#ifdef _WIN32
#include <sys/timeb.h>
#endif
#include <math.h>

defclass  DateTime : Date, Time;

static	short	*Timemark(short int *v)
{
#if	defined(_WIN32)  &&  !defined(__MINGW32__)  &&  !defined(__WINE__)
	SYSTEMTIME	st;
	
	GetLocalTime(&st);
	v[0] = st.wYear;
	v[1] = st.wMonth;
	v[2] = st.wDay;
	v[3] = st.wHour;
	v[4] = st.wMinute;
	v[5] = st.wSecond;
	v[6] = st.wMilliseconds;
#else
#ifdef	PLAN9
	Tm	*a;
	time_t	t;
	time(&t);
	a = localtime(t);
	v[0] = a->year + 1900;
	v[1] = a->mon + 1;
	v[2] = a->mday;
	v[3] = a->hour;
	v[4] = a->min;
	v[5] = a->sec;
	v[6] = 0;
#else   //  all others
	struct	tm	*a;
	time_t		t;
	time(&t);
	a = localtime(&t);
	v[0] = a->tm_year + 1900;
	v[1] = a->tm_mon + 1;
	v[2] = a->tm_mday;
	v[3] = a->tm_hour;
	v[4] = a->tm_min;
	v[5] = a->tm_sec;
	v[6] = 0;
#endif
#endif
	return v;
}

cmeth	gNewDateTime(long dt, long tm)
{
	object	obj = gNew(super);

	gChangeDateValue(obj, dt);
	gChangeTimeValue(obj, tm);
	
	return obj;
}	

cmeth	gNow()
{
	object	obj = gNew(super);
	long	dt, tm;
	short	v[7];

	Timemark(v);
	dt = 10000L * (long) v[0] + 100L * (long) v[1] + (long) v[2];
	tm = 10000000L * (long) v[3] + 100000L * (long) v[4] + 1000L * (long) v[5] + (long) v[6];

	gChangeDateValue(obj, dt);
	gChangeTimeValue(obj, tm);
	
	return obj;
}

imeth	gStringRepValue()
{
	object	s1 = gFormatDate(self, "%Y-%N-%D");
	object	s2 = gTimeStringRepValue(self);
	
	gAppend(s1, (object) " ");
	gAppend(s1, s2);

	gDispose(s2);

	return s1;
}

imeth	gAddHours(long h)
{
	long	nt, dif;
	long	ot = gTimeValue(self);
	long	d = h / 24L;

	h %= 24;
	if (d)
		gAddDays(self, d);
	if (h)
		gAddHours(super, h);
	nt = gTimeValue(self);
	dif = nt - ot;

	if (h > 0  &&  dif < 0)
		gAddDays(self, 1);
	else if (h < 0  &&  dif > 0)
		gAddDays(self, -1);
	
	return self;
}

imeth	gAddMinutes(long m)
{
	long	nt, dif;
	long	ot = gTimeValue(self);
	long	h = m / 60L;

	m %= 60;
	if (h)
		gAddHours(self, h);
	if (m)
		gAddMinutes(super, m);
	nt = gTimeValue(self);
	dif = nt - ot;

	if (m > 0  &&  dif < 0)
		gAddDays(self, 1);
	else if (m < 0  &&  dif > 0)
		gAddDays(self, -1);
	
	return self;
}

imeth	gAddSeconds(long s)
{
	long	nt, dif;
	long	ot = gTimeValue(self);
	long	m = s / 60L;

	s %= 60;
	if (m)
		gAddMinutes(self, m);
	if (s)
		gAddSeconds(super, s);
	nt = gTimeValue(self);
	dif = nt - ot;

	if (s > 0  &&  dif < 0)
		gAddDays(self, 1);
	else if (s < 0  &&  dif > 0)
		gAddDays(self, -1);
	
	return self;
}

imeth	gAddMilliseconds(long l)
{
	long	nt, dif;
	long	ot = gTimeValue(self);
	long	s = l / 1000L;

	l %= 1000;
	if (s)
		gAddSeconds(self, s);
	if (l)
		gAddMilliseconds(super, l);
	nt = gTimeValue(self);
	dif = nt - ot;

	if (l > 0  &&  dif < 0)
		gAddDays(self, 1);
	else if (l < 0  &&  dif > 0)
		gAddDays(self, -1);
	
	return self;
}

imeth	int	gValidDateTime()
{
	return gValidDate(self)  &&  gValidTime(self);
}

imeth	gDateTimeValues(long *dt, long *tm)
{
	*dt = gDateValue(self);
	*tm = gTimeValue(self);
	return self;
}

imeth	gChangeDateTimeValues(long dt, long tm)
{
	gChangeDateValue(self, dt);
	gChangeTimeValue(self, tm);
	return self;
}

imeth	gDateTimeDifference : dtDiff (dt, long *ddr, long *tdr)
{
	object	dobj, tobj;
	long	dd, td;
	
	ChkArgTyp(dt, 2, DateTime);
	dobj = gNewWithLong(Date, gDateValue(dt));
	tobj = gNewWithLong(Time, gTimeValue(dt));
	dd = gDifference(self, dobj);
	td = gTimeDifference(self, tobj);

	gDispose(dobj);
	gDispose(tobj);
	
	if (dd > 0L  &&  td < 0L) {
		dd--;
		td += 86400000L;	/* Number of milliseconds in a day  */
	} else if (dd < 0L  &&  td > 0L) {
		dd++;
		td -= 86400000L;
	}
	if (ddr)
		*ddr = dd;
	if (tdr)
		*tdr = td;
	
	return self;
}

imeth	int	gHash()
{
	double	t1, t2;

	t1 = .6125423371 * (unsigned long) gDateValue(self);
	t1 = t1 < 0.0 ? -t1 : t1;

	t2 = t1 + .6125423371 * (unsigned long) gTimeValue(self);
	t2 = t2 < 0.0 ? -t2 : t2;

	return (int) (BIG_INT * (t2 - floor(t2)));
}

imeth	int	gCompare(obj)
{
	long	dd, td;
	
	ChkArg(obj, 2);
	dtDiff(self, obj, &dd, &td);
	if (dd < 0L  ||  td < 0L)
		return -1;
	if (dd > 0L  ||  td > 0L)
		return 1;
	return 0;
}

imeth	gChangeLongValue(long val)
{
	gChangeDateValue(super, val);
	gChangeTimeValue(self, 0L);
	
	return self;
}

imeth	gChangeValue(object from)
{
	gChangeDateValue(super, gDateValue(from));
	gChangeTimeValue(self, gTimeValue(from));
	return self;
}

imeth	gFormatDateTime(char *dtbuf, char *tmbuf)
{
	object	s1 = gFormatDate(self, dtbuf);
	object	s2 = gFormatTime(self, tmbuf);

	gAppend(s1, s2);
	gDispose(s2);	
	
	return s1;
}







