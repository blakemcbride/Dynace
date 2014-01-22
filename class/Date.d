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




defclass  Date : LongInteger  {

class:
	int	cFixType;	// 0 does nothing, 1 goes to last day of month, 2 goes to next day of next month
	
init:	init_class;
};

#include <string.h>
#include <math.h>
#ifndef	__COSMIC__
#include <time.h>
#endif



static	gLongValue_t		longValue;/*  local cache LongValue method  */
static	gChangeLongValue_t	changeLongValue;

static	long	fixInvalidDate(long dt);



static	char	*Dtfmt(long d, char *s)	/*  format date	yyyymmdd  to mm/dd/yy  */
{
	int	i;
	char	v[7];

	s[8] = '\0';
/*
	if (d >= 20000101L  &&  d < 20900101L)
		d -= 10000L;
	if (d < 19000101L  ||  d > 19991231L)	{
		for (i=0 ; i != 8 ; )	s[i++] = '*';
		return(s);
	}
*/
	if (d < 101L)  {
		for (i=0 ; i != 8 ; )
			s[i++] = ' ';
		return(s);
	}
	d %= 1000000L;
	sprintf(v, "%6ld", d);
	s[0] = v[2] == '0' ? ' ' : v[2];
	s[1] = v[3];
	s[2] = '/';
	s[3] = v[4];
	s[4] = v[5];
	s[5] = '/';
	s[6] = v[0] == ' ' ? '0' : v[0];
	s[7] = v[1] == ' ' ? '0' : v[1];
	return(s);
}

#if 0

static	char	*Dtfmt4(long d, char *s)	/*  format date	yyyymmdd  to mm/dd/yyyy	 */
{
	int	i;
	char	v[9];

	if (d <= 0L  ||  d > 99991231L)  {
		for (i=0 ; i != 10 ; )
			s[i++] = ' ';
		s[i] = '\0';
		return(s);
	}
	sprintf(v, "%8ld", d);
	s[0] = (v[4] ==	'0') ? ' ' : v[4];
	s[1] = v[5];
	s[2] = '/';
	s[3] = v[6];
	s[4] = v[7];
	s[5] = '/';
	s[6] = v[0];
	s[7] = v[1];
	s[8] = v[2];
	s[9] = v[3];
	s[10] =	'\0';
	return(s);
}

#endif

static	long	Jul(long x)		/*  converts cal yyyymmdd to julian day	 */
{
	long	d, y;
	long	m;

	if (x <= 0L)		return(x);
	y = x /	10000L;
	m = (x % 10000L) / 100L;
	d = x %	100L;
	d += (long) (.5 + (m - 1L) * 30.57);
	if (m >	2L)  {
		d--;
/*  Had to convert to the following lines because of bug in MSVC 1.0
		if (0L != y % 400L  &&  (0L != y % 4L  ||  0L == y % 100L))
			d--;
*/
		if (0L != y % 400L  &&  0L != y % 4L)
			d--;
		else if (0L != y % 400L  &&  0L == y % 100L)
			d--;
	}
	d += (long) (365.25 * --y);
	d += y / 400L;
	d -= y / 100L;
	return(d);
}

static	long Cal(long d)		/*  converts julian date to calander yyyymmdd */
{
	long	y, m, t;

	if (d <= 0L)		return(d);
	y = (long)(1.0 + d / 365.2425);
	t = y -	1L;
	d -= (long) (t * 365.25);
	d -= t / 400L;
	d += t / 100L;
	if (d >	59L  &&	 0L != y % 400L	 &&  (0L != y %	4  ||  0L == y % 100L))
		d++;
	if (d >	60L)	
		d++;
	m = (long)((d + 30L) / 30.57);
	d -= (long) floor(.5 + (m - 1L)	* 30.57);
	if (m == 13)  {
		m = 1;
		++y;
	}  else  if (!m)  {
		m = 12;
		--y;
	}
	return 10000L * y + m * 100L + d;
}

static	char	*Dyofwk(long d, char *s)		/* day of week routine	*/
{
	int	i;
	char	*p;

	if (d <= 0L)
		if (s)  {
			*s = '\0';
			return(s);
		}  else
			return("");
	i = Jul(d) % 7L;
	switch	(i)  {
	case  0:	p = "Sunday";		break;
	case  1:	p = "Monday";		break;
	case  2:	p = "Tuesday";		break;
	case  3:	p = "Wednesday";	break;
	case  4:	p = "Thursday";		break;
	case  5:	p = "Friday";		break;
	case  6:	p = "Saturday";		break;
	default:	p = "";			break;
	}
	if (s)  {
		strcpy(s, p);
		return(s);
	}  else
		return(p);
}

static	char	*Mmofyr(int m, char *s)		/* month of year  */
{
	char	*p;
	
	if (m <= 0)
		if (s)  {
			*s = '\0';
			return(s);
		}  else
			return("");
	m = (m - 1) % 12;
	switch (m)  {
	case  0:	p = "January";		break;
	case  1:	p = "February";		break;
	case  2:	p = "March";		break;
	case  3:	p = "April";		break;
	case  4:	p = "May";		break;
	case  5:	p = "June";		break;
	case  6:	p = "July";		break;
	case  7:	p = "August";		break;
	case  8:	p = "September";	break;
	case  9:	p = "October";		break;
	case 10:	p = "November";		break;
	case 11:	p = "December";		break;
	default:	p = "";			break;
	}
	if (s)  {
		strcpy(s, p);
		return(s);
	}  else
		return(p);
}

#ifndef	__COSMIC__

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

static	long _Today(void)
{
	short	v[7];

	Timemark(v);
	return 10000L * (long) v[0] + 100L * (long) v[1] + (long) v[2];
}

#endif

static	long	Date_ymd(long date, int *year, int *month, int *day)
{
	register int  temp;

	temp = (int) (date % 10000L);

	if (year)		*year  = (int) (date / 10000L);
	if (month)		*month = temp /	100;
	if (day)		*day   = temp %	100;
	return(date);
}

static	long	Ymd_date(int year, int month, int day)
{
	return((long) year * 10000L + (long) month * 100L + (long) day);
}

static	char	*Num_sufx(int d)
{
	register int   i = d % 10;

	if (i == 1 && d	!= 11)
		return "st";
	else if	(i == 2	&& d !=	12)
		return "nd";
	else if	(i == 3	&& d !=	13)
		return "rd";
	else	return "th";
}

static	char	*Time2(char *buf)
{
	short	v[7], t=0;

	Timemark(v);
	if (v[3] >= 12)	 {
		t = 1;
		if (v[3] > 12)		v[3] -=	12;
	}
	sprintf(buf, "%2d:%2d %s",
		(int) v[3], (int) v[4], t ? "PM" : "AM");
	if (buf[3] == ' ')
		buf[3] = '0';
	return(buf);
}

static	char	*DateFmt(char *buf, long dt, char *msk)
{
	int	y, m, d;
	char	tmp[20], *bp = buf;

	if (dt <= 0L)  {
		*buf = '\0';
		return(buf);
	}
	Date_ymd(dt, &y, &m, &d);
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
			case 'W':		/*  Day of week  */
			case 'w':
				strcpy(bp, Dyofwk(dt, NULL));
				bp += strlen(bp);
				msk++;
				break;
			case 'M':	/*  Month of the year  */
				strcpy(bp, Mmofyr(m, NULL));
				bp += strlen(bp);
				msk++;
				break;
			case 'm':	/*  Month name abreveation  */
				strcpy(bp, Mmofyr(m, NULL));
				bp += 3;
				msk++;
				break;
			case 'd':	/*  Day of the month  */
				sprintf(bp, "%d", d);
				bp += strlen(bp);
				msk++;
				break;
			case 'D':	/*  Day of the month  */
				sprintf(bp, "%02d", d);
				for ( ; *bp ; ++bp)
					if (*bp == ' ')
						*bp = '0';
				msk++;
				break;
			case 'y':		/*  Year  93  */
				sprintf(bp, "%02d", y%100);
				for ( ; *bp ; ++bp)
					if (*bp == ' ')
						*bp = '0';
				msk++;
				break;
			case 'Y':	/*  Year  1993  */
				sprintf(bp, "%4d", y);
				bp += strlen(bp);
				msk++;
				break;
			case 'S':	/*  Numeric Suffix  */
			case 's':
				strcpy(bp, Num_sufx(d));
				bp += strlen(bp);
				msk++;
				break;
			case 'n':	/*  Month number  */
				sprintf(bp, "%d", m);
				bp += strlen(bp);
				msk++;
				break;
			case 'N':	/*  Month number  */
				sprintf(bp, "%02d", m);
				bp += strlen(bp);
				msk++;
				break;
			case 'T':	/*  current time  */
			case 't':
				strcpy(bp, Time2(tmp));
				bp += strlen(bp);
				msk++;
				break;
			}
	*bp = '\0';
	return(buf);
}

static	long	Add_days(long date, long days)
{
	if (date <= 0L)		return(0L);
	return Cal(days + Jul(date));
}

static	long	Add_mon(long date, int months)
{
	int	y, m, d;
	long	mon;

	if (date <= 0L)		return(0L);
	Date_ymd(date, &y, &m, &d);
	mon = 12L * (long) y + (long) m	+ (long) months	- 1L;
	y = mon	/ 12L;
	m = 1L + mon % 12L;
	date = Ymd_date(y, m, d);

	return fixInvalidDate(date);
}

static	long	Add_year(long date, int years)
{
	int	y, m, d;

	if (date <= 0L)		return(0L);
	Date_ymd(date, &y, &m, &d);
	y += years;
	date = Ymd_date(y, m, d);
	return fixInvalidDate(date);
}

static	int	Valid_date(long d)
{
	return d == Cal(Jul(d));
}

static	long	fixInvalidDate(long dt)
{
	if (!cFixType  ||  Valid_date(dt))
		return dt;

	switch(cFixType) {
	case 1:
		dt = Cal(Jul(dt));
		dt = (dt - (dt % 100L)) + 1L;
		dt = Add_days(dt, -1L);
		break;
	case 2:
		dt = Cal(Jul(dt));
		break;
	}
	return dt;
}

static	long	Date_dif(long d1, long d2)
{
	return Jul(d1) - Jul(d2);
}

cmeth	gNewWithStr(char *dtStr)
{
	//this will parse a 2005-07-18 date into a date
	//or  2005/07/18 date into a date
	//the 0 based padding is required
	//for now, any other input is bad!
	long	dt = 0;


	if (dtStr && (strlen(dtStr)>=10)) {
		int year=(dtStr[0]-'0')*1000+(dtStr[1]-'0')*100+(dtStr[2]-'0')*10+(dtStr[3]-'0');
		int month=(dtStr[5]-'0')*10+(dtStr[6]-'0');
		int day=(dtStr[8]-'0')*10+(dtStr[9]-'0');

		dt = Ymd_date(year, month, day);
	}

	return gNewWithLong(self, dt);
}

imeth	gDayName()
{
	return gNewWithStr(String, Dyofwk(longValue(self), NULL));
}

imeth	gMonthName()
{
	int	m;
	Date_ymd(longValue(self), NULL, &m, NULL);
	return gNewWithStr(String, Mmofyr(m, NULL));
}

imeth	gFormatDate, <vFormat> (char *msk)
{
	char	buf[80];

	return gNewWithStr(String, DateFmt(buf, longValue(self), msk));
}

imeth	gStringRepValue()
{
	char	buf[15];

	return gNewWithStr(String, Dtfmt(longValue(self), buf));
}

cmeth	gToday()
{
	return gNewWithLong(self, _Today());
}

imeth	gAddDays(long days)
{
	return changeLongValue(self, Add_days(longValue(self), days));
}

imeth	gAddMonths(int months)
{
	return changeLongValue(self, Add_mon(longValue(self), months));
}

imeth	gAddYears(int years)
{
	return changeLongValue(self, Add_year(longValue(self), years));
}

imeth	int	gValidDate()
{
	return Valid_date(longValue(self));
}

imeth	long	gDifference(dt)
{
	ChkArgTyp(dt, 2, Date);
	return Date_dif(longValue(self), longValue(dt));
}

cmeth	long	gCalToJul(long dt)
{
	return Jul(dt);
}

cmeth	long	gJulToCal(long dt)
{
	return Cal(dt);
}

imeth	long	gJulian()
{
	return Jul(longValue(self));
}

imeth	gChangeDateValue(long dt)
{
	changeLongValue(self, dt);
	return self;
}

imeth	long	gDateValue()
{
	return longValue(self);
}

//  The following is needed to support ODBC across different databases.
//  Some databases support dates and others only datetimes.  In order to
//  transfer data between them this method is needed.

imeth	gChangeDateTimeValues(long dt, long tm)
{
	changeLongValue(self, dt);
	return self;
}

static	void	init_class(void)
{
	longValue = imcPointer(CLASS, gLongValue);
	changeLongValue = imcPointer(CLASS, gChangeLongValue);
	
	cFixType = 1;
}

cmeth	int	gFixInvalidDate(int mode)
{
	int	pm = cFixType;

	cFixType = mode;
	return pm;
}




