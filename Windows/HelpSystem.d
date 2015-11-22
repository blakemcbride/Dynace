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




#include "logfile.h"
#include "hdlcache.h"


defclass  HelpSystem  {
 class:
	cFile;
	cMainWindow;
	int	cUsed;		/*  1=help system called		*/
	cTopic;			/*  current topic based on context	*/
	cPrevTopic;		/*  previous topic			*/
};

cmeth	gInitHelp(wind)
{
	if (!cMainWindow)
		cMainWindow = wind;
	return self;
}
		
cmeth	gFreeHelp(wind)
{
	if (wind == cMainWindow)  {
		if (cUsed)  {
			WinHelp(gHandle(cMainWindow), gStringValue(cFile), HELP_QUIT, 0L);
			cUsed = 0;
		}
		cMainWindow = NULL;
	}
	return self;
}

cmeth	gHelpFile(char *file)
{
	if (cFile)
		gDispose(cFile);
	cFile = file ? gNewWithStr(String, file) : NULL;
	return self;
}

cmeth	gHelpContents()
{
	int	r = 0;

	if (cFile  &&  cMainWindow)  {
		r = WinHelp(gHandle(cMainWindow), gStringValue(cFile), HELP_CONTENTS, 0L);
		cUsed = 1;
	}
	return r ? self : NULL;
}

cmeth	gHelpTopic(char *topic)
{
	char	cmd[90], *file;
	int	r = 0;

	if (cFile  &&  cMainWindow)  {
		sprintf(cmd, "JumpID(\"%s\",\"%s\")", file=gStringValue(cFile), topic);
		WinHelp(gHandle(cMainWindow), file, HELP_FORCEFILE, 0L);
		r = WinHelp(gHandle(cMainWindow), file, HELP_COMMAND, (ULONG_PTR)cmd);
		cUsed = 1;
	}
	return r ? self : NULL;
}

cmeth	char	*gSetTopic(char *topic)
{
	if (cPrevTopic)
		gDispose(cPrevTopic);
	cPrevTopic = cTopic;
	if (topic  &&  *topic)
		cTopic = gNewWithStr(String, topic);
	else
		cTopic = NULL;
	return cPrevTopic ? gStringValue(cPrevTopic) : NULL;
}

cmeth	char	*gGetTopic()
{
	return cTopic ? gStringValue(cTopic) : NULL;
}

cmeth	gHelpInContext()
{
	return cTopic ? gHelpTopic(self, gStringValue(cTopic)) : gHelpContents(self);
}





