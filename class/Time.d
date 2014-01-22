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




defclass  Time {
	long	iVal;	//  HHMMSSmmm
};

#include <string.h>
#include <math.h>
#ifndef	__COSMIC__
#include <time.h>
#endif
#ifdef _WIN32
#include <sys/timeb.h>
#endif

#define	GET_HOURS(t)		(t / 10000000L)
#define	GET_MINUTES(t)		((t / 100000L) % 100L)
#define	GET_SECONDS(t)		((t / 1000L) % 100L)
#define	GET_MILLI(t)		(t % 1000)
#define	BUILD_TIME(h, m, s, l)	(((long) h * 10000000L) + ((long) m * 100000L) + ((long) s * 1000) + (long) l)

static	long	Time_hmsl(long tm, int *h, int *m, int *s, int *l)
{
	if (h)
		*h = GET_HOURS(tm);
	if (m)
		*m = GET_MINUTES(tm);
	if (s)
		*s = GET_SECONDS(tm);
	if (l)
		*l = GET_MILLI(tm);
	
	return tm;
}

static	long	toMilli(long tm)
{
	int	h, m, s, l;

	Time_hmsl(tm, &h, &m, &s, &l);

	return ((long) h * 3600000L) + ((long) m * 60000L) + ((long) s * 1000L) + (long) l;
}
 
static	long	fromMilli(long ms)
{
	int	h, m, s, l;

	ms %= 86400000L;		// Reduce the number of milliseconds to less than a day
	if (ms < 0)
		ms += 86400000L;	// Could be a negative value, make the correct positive value

	h = (int) (ms / 3600000L);	// The rest now falls into place.
	ms %= 3600000L;
	m = (int) (ms / 60000L);
	ms %= 60000L;
	s = (int) (ms / 1000L);
	l = (int) (ms % 1000L);

	return BUILD_TIME(h, m, s, l);
}

static	long	Add_hours(long tm, long hrs)
{
	long	ms = toMilli(tm);
	long	ha = hrs * 3600000L;

	return fromMilli(ms + ha);
}

static	long	Add_minutes(long tm, long min)
{
	long	ms = toMilli(tm);
	long	ma = min * 60000L;

	return fromMilli(ms + ma);
}

static	long	Add_seconds(long tm, long sec)
{
	long	ms = toMilli(tm);
	long	sa = sec * 1000L;

	return fromMilli(ms + sa);
}

static	long	Add_milliseconds(long tm, long msec)
{
	return fromMilli(toMilli(tm) + msec);
}

static	int	Valid_time(long tm)
{
	int	h, m, s, l;

	Time_hmsl(tm, &h, &m, &s, &l);

	if (h < 0 || h >= 24)
		return 0;
	if (m < 0 || m >= 60)
		return 0;
	if (s < 0 || s >= 60)
		return 0;
	if (l < 0 || l >= 1000)
		return 0;
	return 1;
}

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

static	long	getNow(void)
{
	short	v[7];

	Timemark(v);

	return BUILD_TIME(v[3], v[4], v[5], v[6]);
}

static	char	*Tmfmt(long t, char *s)	/*  format time	hhmmsslll  to hh:mm:ss.lll xx  */
{
	int	h = GET_HOURS(t);
	char	*p;
	
	if (h < 12) {
		if (!h)
			h += 12;
		p = "am";
	} else {
		if (h > 12)
			h -= 12;
		p = "pm";
	}
	
	sprintf(s, "%d:%02d:%02d.%03d%s", h, (int) GET_MINUTES(t), (int) GET_SECONDS(t), (int) GET_MILLI(t), p);
	return s;
}

static	char	*TimeFmt(char *buf, long tm, char *msk)
{
	int	h, m, s, l;
	char	*bp = buf;
	int	temp;

	if (tm < 0L)  {
		*buf = '\0';
		return(buf);
	}
	Time_hmsl(tm, &h, &m, &s, &l);
	while (*msk)
		if (*msk != '%')
			*bp++ = *msk++;
		else
			switch (*++msk)  {
			case '\0':
				break;
			case '%':
			default:
				*bp++ = *msk++;
				break;
			case 'G':
				sprintf(bp, "%02d", h);
				bp += strlen(bp);
				msk++;
				break;
			case 'g':
				sprintf(bp, "%d", h);
				bp += strlen(bp);
				msk++;
				break;
			case 'H':
				if (!h)
					temp = 12;
				else if (h > 12)
					temp = h - 12;
				else
					temp = h;
				sprintf(bp, "%02d", temp);
				bp += strlen(bp);
				msk++;
				break;
			case 'h':
				if (!h)
					temp = 12;
				else if (h > 12)
					temp = h - 12;
				else
					temp = h;
				sprintf(bp, "%d", temp);
				bp += strlen(bp);
				msk++;
				break;
			case 'M':
				sprintf(bp, "%02d", m);
				bp += strlen(bp);
				msk++;
				break;
			case 'm':
				sprintf(bp, "%d", m);
				bp += strlen(bp);
				msk++;
				break;
			case 'S':
				sprintf(bp, "%02d", s);
				bp += strlen(bp);
				msk++;
				break;
			case 's':
				sprintf(bp, "%d", s);
				bp += strlen(bp);
				msk++;
				break;
			case 'L':
				sprintf(bp, "%03d", l);
				bp += strlen(bp);
				msk++;
				break;
			case 'l':
				sprintf(bp, "%d", l);
				bp += strlen(bp);
				msk++;
				break;
			case 'P':
				strcpy(bp, h >= 12 ? "PM" : "AM");
				bp += strlen(bp);
				msk++;
				break;
			case 'p':
				strcpy(bp, h >= 12 ? "pm" : "am");
				bp += strlen(bp);
				msk++;
				break;
			}
	*bp = '\0';
	return(buf);
}

static	long	Time_dif(long t1, long t2)
{
	long	dif;
	int	x = GET_HOURS(t1) - GET_HOURS(t2);

	dif = (long) x * 3600000L;
	x = GET_MINUTES(t1) - GET_MINUTES(t2);
	dif += ((long) x * 60000L);
	x = GET_SECONDS(t1) - GET_SECONDS(t2);
	dif += ((long) x * 1000L);
	dif += (GET_MILLI(t1) - GET_MILLI(t2));

	return dif;
}

cmeth	gNewWithLong(long val)
{
	object	obj = gNew(super);
	accessIVsOf(obj);
	
	iVal = val;
	return(obj);
}

cmeth	gNewWithStr(char *tmStr)
{
	//this will parse a 10:12:59 time
	//the 0 based padding is required
	//for now, any other input is bad!
	long	tm = 0;

	if (tmStr)  {
		int hour=(tmStr[0]-'0')*10+(tmStr[1]-'0');
		int minute=(tmStr[3]-'0')*10+(tmStr[4]-'0');
		int second=(tmStr[6]-'0')*10+(tmStr[7]-'0');
		int milli=0;

		if (strlen(tmStr) > 10)
			milli = (tmStr[9]-'0')*100+(tmStr[10]-'0')*10+(tmStr[11]-'0');

		tm = BUILD_TIME(hour, minute, second, milli);
	}
	return gNewWithLong(self, tm);
}

imeth	int	gHash()
{
	double	t;

	t = .6125423371	* (unsigned long) iVal;
	t = t < 0.0 ? -t : t;
	return (int) (BIG_INT * (t - floor(t)));
}

imeth	int	gCompare(obj)
{
	long	sv, ov;

	ChkArg(obj, 2);
	if ((sv=iVal) < (ov=ivPtr(obj)->iVal))
		return -1;
	else if (sv == ov)
		return 0;
	else
		return 1;
}

imeth	long	gLongValue, gTimeValue ()
{
	return (long) iVal;
}

imeth	void	*gPointerValue()
{
	return (void *) &iVal;
}

imeth	gChangeValue(val)
{
	ChkArg(val, 2);
	iVal = gLongValue(val);
	return self;
}

imeth	gChangeLongValue, gChangeTimeValue (long val)
{
	iVal = (long) val;
	return self;
}

imeth	int	gHours()
{
	return GET_HOURS(iVal);
}

imeth	int	gMinutes()
{
	return GET_MINUTES(iVal);
}

imeth	int	gSeconds()
{
	return GET_SECONDS(iVal);
}

imeth	int	gMilliseconds()
{
	return GET_MILLI(iVal);
}

imeth	gFormatTime(char *msk)
{
	char	buf[80];

	return gNewWithStr(String, TimeFmt(buf, iVal, msk));
}

imeth	gStringRepValue, gTimeStringRepValue()
{
	char	buf[25];

	return gNewWithStr(String, Tmfmt(iVal, buf));
}

cmeth	gNow()
{
	return gNewWithLong(self, getNow());
}

imeth	gAddHours(long h)
{
	iVal = Add_hours(iVal, h);
	return self;
}

imeth	gAddMinutes(long m)
{
	iVal = Add_minutes(iVal, m);
	return self;
}

imeth	gAddSeconds(long s)
{
	iVal = Add_seconds(iVal, s);
	return self;
}

imeth	gAddMilliseconds(long l)
{
	iVal = Add_milliseconds(iVal, l);
	return self;
}

imeth	int	gValidTime()
{
	return Valid_time(iVal);
}

imeth	long	gDifference, gTimeDifference (tm)
{
	ChkArgTyp(tm, 2, Time);
	return Time_dif(iVal, ivPtr(tm)->iVal);
}




