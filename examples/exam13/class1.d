
 /*  Nothing in this file has been changed from the previous example.  */

#include <string.h>

defclass  Class1  {
	char	iName[30];
	int	iCode;
	iData;
};

imeth	gSetName(char *name)
{
	strcpy(iName, name);
	return self;
}

imeth	char	* gGetName : get_name ()
{
	return iName;
}

