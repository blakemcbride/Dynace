



/*
 *
 *	This source code is CONFIDENTIAL and
 *	PROPRIETARY to Blake McBride (blake@mcbride.name). Unauthorized
 *	distribution, adaptation or use	may
 *	be subject to civil and	criminal penalties.
 *
 *	Copyright (c) 1993 Blake McBride (blake@mcbride.name)
 *
 *	ALL RIGHTS RESERVED.
 *
 *
 *
 */


 /*  This example builds on the previous example by the addition of
     two methods which allow users of this class to set and obtain the
     value in the iName instance variable.  */

#include <string.h>

defclass  Class1  {
	char	iName[30];
	int	iCode;
	iData;
};

/*  Methods are declared in a similar fashion to C functions (using
    ANSI prototypes) except as noted as follows.

   'imeth' is used to introduce instance methods with compile time argument
    checking enabled.  Other method introductory commands are 'cmeth' which
    introduces class methods with compile time argument checking,  'ivmeth'
    which introduces variable argument instance methods without compile time
    checking,  and 'cvmeth' which defines variable argument class methods
    without compile time argument checking.

    Since no return type is declared, Dynace defaults it to type 'object'.

    The name of the generic this method will be associated with is 'gSetName',
    and since there is no explicit method name given, Dynace will just default
    to 'm_gSetName'.  This is quite convenient since you rarely reference a
    method directly.  If, however, a method is accessed directly (only
    possible from within the same source file), a method name should be
    explicitly declared for use.  The next example will show how to give a
    method an explicit name.  

    Although no naming convention is enforced by Dynace, the suggested
    convention is to use a leading 'g' followed by an uppercase letter to
    designate a generic (instance or class) which is using compile-time
    argument checking.  A leading 'v' followed by an uppercase letter is
    used to indicate a variable argument generic, or one whos arguments are
    not checked at compile time.  The 'g' stands for generic,  and the 'v'
    stands for variable argument generic.

    A method may also be associated with a group of generics.

    The first argument to ALL methods is 'object self'.  Since it is always
    the same,  Dynace defaults this declaration and the programmer need
    not explicitly declare it.

    This specific generic (gSetName) takes an additional argument 'char *name'.

    Notice how the instance variables associated with the instance passed to
    the method (as the first argument) are immediatly accessable.  This is
    one reason for the instance variable nameing convension.  Otherwise,
    it may be difficult to distinguish betweem instance variables and local
    variables.

    This method simply copies the argument passed to the specified instance
    variable.

    It is common practice, although not strictly required, under Dynace
    to return an object from methods.  If no meaningful return value is
    available, simply return the instance passed (self).
*/

imeth	gSetName(char *name)
{
	strcpy(iName, name);
	return self;
}

/*  This next method is defined as above except as follows:

    This new method, named get_name, is explictly defined to return a char *.
    Also, the method is explicitly associated with the gGetName generic.
    If you wanted to associated it with several generics you would use the
    following syntax:

    	imeth  char  * gGetName, gAnotherGeneric, gAbc : get_name ()

    This associates the method with all three generics.

    Since no arguments are declared, this method will only take the single,
    default, argument of 'object self'.

    This method returns a pointer to the iName instance variable associated
    with the instance passed.
*/

imeth	char	*gGetName : get_name ()
{
	return iName;
}


/*
 *
 *	This source code is CONFIDENTIAL and
 *	PROPRIETARY to Blake McBride (blake@mcbride.name). Unauthorized
 *	distribution, adaptation or use	may
 *	be subject to civil and	criminal penalties.
 *
 *	Copyright (c) 1993 Blake McBride (blake@mcbride.name)
 *
 *	ALL RIGHTS RESERVED.
 *
 *
 *
 */


