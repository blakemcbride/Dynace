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


#include "generics.h"
#include "scheme.h"
#include "barext.h"


static	object	Win;


void	BAR_message(char *s)
{
	if (s  &&  Win)
		gPuts(Win, s);
}	

int	BAR_getch()
{
	int	c;

	if (Win) {
		BAR_message("(or type 'Q' to quit)");
		gWaitCursor(Win, 0);
		gSetFocus(Win);
		c = gGetch(Win);
		if (c == 'q'  ||  c == 'Q')
			BAR_error("Quiting.\n");
		gWaitCursor(Win, 1);
	}
	return c;
}


/*  usage:  (BAR  wind  arg1  arg2 ... )       */

#define	MAX_ARGS	20

SCHEME_FUNCTION(bar)
{
	static	char	fun[] = "BAR";
	int	i;
	char	*vec[MAX_ARGS];

	if (argc < 3  ||  argc > MAX_ARGS)
		scheme_wrong_count(fun, 3, MAX_ARGS, argc, argv);
	SCHEME_ARG_VCOUNT(3);
	SCHEME_CHK_ARG_DYNACE(0);
	Win = (object) SCHEME_PTR_VAL(argv[0]);
	vec[0] = "MzScheme:BAR";
	for (i=1 ; i < argc ; i++) {
		SCHEME_CHK_ARG_STRING(i);
		vec[i] = SCHEME_STR_VAL(argv[i]);
	}
	vec[i] = NULL;
	i = BAR_main(vec);
	Win = NULL;
	return scheme_make_integer(i);
}


void	Scheme_init_BAR(void)
{
	ADD_FUN(BAR, bar);
}
