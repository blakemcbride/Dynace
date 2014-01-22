
/*  Copyright 1995 Algorithms Corporation  */


#include "generics.h"


int	main(int argc, char *argv[])
{
	char	buf[256];

	InitDynace(&argc);
	if (argc != 2)
		gError(Object, "Usage:  CWDS  file");

#if 0
	sprintf(buf, "(load \"%s.scm\")", argv[1]);
	gExecuteString(Scheme, buf);
#else
	sprintf(buf, "%s.scm", argv[1]);
	gExecuteSchemeFile(Scheme, buf);
#endif
	return 0;
}

void	Scheme_init_app(void)
{
}	


void	_link_WDS_main(void)   //  this function prevents my WinMain from linking in - hence a console app
{
}
