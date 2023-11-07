
#include <string.h>

defclass  Class1  {
	char	iName[30];
	int	iCode;
	iData;
  class:
  	int	cNumInstances;
};


cmeth	gNewWithInt(int code)
{
	/*  Declare an object which will hold the new instance object being
	    created.  */
	object	obj;

	/*  Declare a pointer which will point to the instance variables
	    associated with the new instance.  'ivType' refers to a C
	    structure which looks like the instance variable structure
	    defined in the defclass.  The name of the pointer must be 'iv'.
	    This is so Dynace knows how to access the instance variables
	    when they are refered to without the normal C iv-> construct.  */
	ivType	*iv;

	/*  Increment the count as before.  */
	cNumInstances++;

	/*  Create the new instance object as before, only this time put it
	    in obj.  */
	obj = gNew(super);

	/*  Gain access to a pointer to the locally defined instance variables
	    associated with obj and assign it to iv.  Note that this procedure,
	    which is required to access instance variables, is performed
	    automatically on the self object on instance methods.  That is
	    why instance methods may directly access instance variables
	    associated with their self argument without the need for the
	    ivPtr or ivsPtr macros.  */
	iv = ivPtr(obj);

	/*  Since iv is correctly updated it is now safe to directly access
	    the instance variables associated with obj - update iCode with
	    the 'code' value passed to this method.  */
	iCode = code;

	/*  Since we are now through allocating and initializing the new
	    object, simply return it.  */ 
	return obj;
}

/*  Create an instance method to allow an external program to access the
    value in iCode in order to test our New method.  */
    
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





