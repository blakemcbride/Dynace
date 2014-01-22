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


#include <stdio.h>
#include <ctype.h>
#include <devices.h>

#define	MAXOP 20
#define	NUMBER '0'
#define	TOOBIG '9'
#define	MAXVAL 1000

static	int error = 0;
static	double storage[256];

static	int sp = 0;			/* stack pointer  */
static	double val[MAXVAL];

static	void	clear(void);
static	void	pstack(char *buf, int base, int places);
static	void	erasev(void);
static	void	listv(int base, int places, char *buf);


main()
{
	int type, places = 2, base = 10;
	char s[MAXOP], buf[81];
	double op1, op2, aton(), pop(),	push(),	sumall(), pow();

	while ((type = getop(s,	MAXOP))	!= EOF)	 {
		type = isupper(type) ? tolower(type) : type;
		switch (type)  {
		case NUMBER:		/* push	number	*/
			push(aton(s, base));
			break;
		case '+':		/* add	*/
			push(pop() + pop());
			break;
		case '*':		/* multiply  */
			push(pop() * pop());
			break;
		case '-':		/* subtract  */
			op2 = pop();
			push(pop() - op2);
			break;
		case '/':		/* divide  */
			op2 = pop();
			if (op2	!= 0.0)
				push(pop() / op2);
			else
				printf("zero divisor popped\n");
			break;
		case '%':		/*  modulo  */
			op2 = pop();
			if (op2	!= 0.0)
				push((double) ((int) pop() % (int) op2));
			else
				printf("zero divisor popped\n");
			break;
		case '^':		/* power  */
			op2 = pop();
			push(pow(pop(),	op2));
			break;
		case '=':		/* print top value  */
			push(op2=pop());
			if (error)  {
				error =	0;
				clear();
				break;
			}
			printf("\t%s\n", Nfmtb(op2, base, "C", 0, places, buf));
			break;
		case 'a':		/* addup all elements  */
			op2 = sumall();
			clear();
			push(op2);
			break;
		case 'b':		/* set base	*/
			op1 = pop();
			if ((int) op1 <	2  ||  (int) op1 > 36)
				printf("Invalid base - Not changed.\n");
			else
				base = op1;
			break;
		case 'c':		/* clear the stack  */
			clear();
			break;
		case 'd':		/* dup	 */
			op2 = pop();
			push(op2);
			push(op2);
			break;
		case 'e':		/* erase all variables	*/
			erasev();
			break;
		case 'k':		/* number of decimal places  */
			places = pop();
			break;
		case 'l':		/* list	stack  */
			pstack(buf, base, places);
			break;
		case 'm':		/* reverse sign	of top element	*/
			push(-pop());
			break;
		case 'n':		/* push	stack size  */
			push((double) sp);
			break;
		case 'p':		/* pop top entry  */
			pop();
			break;
		case 'q':		/* quit	 */
			exit();
			break;
		case 'r':		/* recall storage  */
			type = getchar();
			push(storage[type]);
			break;
		case 's':		/* save	storage	 */
			type = getchar();
			storage[type] =	pop();
			break;
		case 't':		/* total all elements  */
			push(sumall());
			break;
		case 'v':		/* list	variables  */
			listv(base, places, buf);
			break;
		case 'x':		/* exchange top	two  */
			op1 = pop();
			op2 = pop();
			push(op1);
			push(op2);
			break;
		case TOOBIG:
			printf("%.20s  ... is too long\n", s);
			break;
		default:
			printf("unknown command %c\n", type);
			break;
		}
		if (error)  {
			error =	0;
			clear();
		}
	}
	load_console_extint();
	return(0);
}

double push(f)
double f;
{
	if (sp < MAXVAL)
		return(val[sp++] = f);
	else	{
		printf("error: stack full\n");
		clear();
		error =	1;
		return(0);
		}
}

double pop()
{
	if (sp > 0)
		return(val[--sp]);
	else	{
		printf("error: stack empty\n");
		error =	1;
		clear();
		return(0);
	}
}

static	void	clear(void)
{
	sp = 0;
}

static	void	pstack(char *buf, int base, int places)
{
	register int p = 0;

	printf("\n");
	while (p != sp)
		printf("\t%s\n", Nfmtb(val[p++], base, "C", 41,	places,	buf));
	printf("\n");
}

double aton(v, b)		/*  convert string v to	double	*/
register char  v[];
int	b;			/* base	*/
{
	register int	i, a;
	double	n=0.0, p=1.0;
	int	s=1;

	if (!v)		return 0.0;
	for (i=0 ; isspace(v[i]) ; i++);
	if (v[i]=='+' || v[i]=='-')	s = (v[i++]=='+') ? 1 :	-1;
	for (; (a = valu(v[i], b)) != -1 ; i++)
		n = b *	n + a;
	if (v[i] == '.')
		for (i++ ; (a =	valu(v[i], b)) != -1 ; i++)  {
			n = b *	n + a;
			p *= b;
		}
	return(s * n / p);
}

int	valu(c,	b)
char	c;
int	b;
{
	int	n;

	if (isdigit(c))		n = c -	'0';
	else  if (isalpha(c))	n = toupper(c) - ('A' -	10);
	else  if (c  &&	 c != '.')  {
		printf("%c is not a valid numeric\n", c);
		return(-1);
	}  else	return(-1);
	if (n >= b)  {
		printf("%c is not a digit in base %d\n", c, b);
		return(-1);	/* not in correct base */
	}
	return(n);
}

getop(s,lim)
char s[];
int lim;
{
	int i, c;

	while ((c = getchar()) == ' ' || c == '\t' || c	== '\n');
	if (c != '-' &&	c != '.' && !isdigit(c))	return(c);
	if (c == '-')  {
		c = getchar();
		if (!isdigit(c)	 &&  c != '.')	{
			ungetc(c, stdin);
			return((int) '-');
		}
		s[0] = '-';
		s[1] = c;
		i = 2;
	}  else	 {
		s[0] = c;
		i=1;
	}
	while (1)  {
		c = getchar();
		if (!isalnum(c))	break;
		if (i <	lim)	s[i++] = c;
	}
	if (c == '.') {
		if (i <	lim)	s[i++] = c;
		while (1)  {
			c = getchar();
			if (!isalnum(c))	break;
			if (i <	lim)	s[i++] = c;
		}
	}
	if (i <	lim) {
		ungetc(c,stdin);
		s[i] = '\0';
		return(NUMBER);
	}  else	 {
		while (c != '\n' && c != EOF)
			c = getchar();
		s[lim-1] = '\0';
		return(TOOBIG);
	}
}

double sumall()
{
	int	tsp;
	double	r = 0.0;

	for (tsp = sp-1	; tsp >= 0 ;)
		r += val[tsp--];
	return(r);
}

static	void	erasev(void)
{
	register int	 i;

	for (i=0 ; i !=	256 ; )
		storage[i++] = 0.0;
}

static	void	listv(int base, int places, char *buf)
{
	register int	 i;

	printf("\n");
	for (i=0 ; i !=	256 ; i++)
		if (storage[i] != 0.0)
			printf("\t%c = %s\n", (char) i,	Nfmtb(storage[i], base,	"C", 0,	places,	buf));
	printf("\n");
}

char	*Getenv(char *v)   /*  resolve a link issue  */
{
	extern	char *getenv();
	return getenv(v);
}


