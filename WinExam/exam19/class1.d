

defclass  Class1  {
	char	iName[30];
	int	iCode;
	iData;
  class:
  	int	cNumInstances;
};

cvmeth	vNew(int code)
{
	object	obj;
	ivType	*iv;

	cNumInstances++;
	obj = vNew(super self);
	iv = ivPtr(obj);
	iCode = code;
	return obj;
}

imeth	int	gGetCode()
{
	return iCode;
}

cmeth	int	gNumInstances()
{
	return cNumInstances;
}

imeth	object	gDeepDispose, gDispose ()
{
	cNumInstances--;
	gDispose(super self);
	return NULL;
}

imeth	gSetName(char *name)
{
	strcpy(iName, name);
	return self;
}

imeth	char	* gGetName : get_name ()
{
	return iName;
}

