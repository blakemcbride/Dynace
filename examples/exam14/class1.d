
 /*  This example adds a class variable to the previous examples.
     Class variables are shared by all instances of a class, that is,
     there's only copy and it is associated to the class as a whole.

     Notice the class: declaration.  After this declaration everything
     that follows is a class variable.  There is also a similar declaration
     used for instance variables called instance:   However, this declaration
     is rarely needed because it is the default state when defining a new
     class.

     Note the use of the standard nameing convension of nameing class
     variables with a leading 'c' (for class) followed by an upper case
     letter.  Again, this is just a convention which is not required by
     Dynace.

     Like the instance variable section, any variable whose type is not
     explicitly declared will default to type 'object'.
*/

#include <string.h>

defclass  Class1  {
	char	iName[30];
	int	iCode;
	iData;
  class:
  	int	cNumInstances;
};

/*  Define the New class method.  Since it is a class method it must be
    introduced with 'cmeth' or 'cvmeth'.

    Since no return type is declared 'object' is assumed.

    Since no arguments are declared, this method will only have the minimum,
    and default argument 'object self'.  Note, however, that in the case of
    class methods, 'self' will always refer to the class object as apposed
    to an instance object as with instance methods.

    The first line increments the cNumInstances class variable in order
    to note the new instance being created.  Notice how the class variables
    are directly accessable within the method.  This is also true within
    instance methods.

    The second line calls the superclass of this class (in this case 'Object')
    with the generic 'gNew' and a sole argument 'self' in order to create
    an instance of the current class (pointed to by self).  This is done
    because only the New method associated to the Object class actually
    knows how to allocate a new instance object.  Once the new instance object
    is allocated (with the shown super call) it may be further initialized.

    In this case the new instance object is simply returned.
*/

cmeth	gNew()
{
	cNumInstances++;
	return gNew(super);
}

/*  This next class method simply returns the value of the cNumInstances
    class variable.  Like all methods its first argument is assumed to be
    'object self'.  And since this is a class method self will always refer
    to the class object.  */


cmeth	int	gNumInstances()
{
	return cNumInstances;
}

/*  This next instance method is used to deallocate or free an instance
    of this class.  It is being associated to two generics (gDeepDispose
    and gDispose).

    The first thing it does is decrement cNumInstances in order to keep
    that variable in sync since this method will free the instance passed
    (as the first argument).

    The second line calls the gDispose generic with the single argument,
    self on the superclass of this class (which happens to be Object).
    This is done because the Dispose method associated with the Object class
    is the only routine which has the ability to dispose of a Dynace object.

    The final line return a NULL.  This is a convention which allows the
    code which called the gDispose generic to NULL out the object pointer
    with the same expression.

    Note that prior to the definition of this method (as in the previous
    examples), when the user evoked the gDispose generic on an instance of
    this class, since no Dispose method was defined, the system automatically
    executed the Dispose method associated with the Object class because
    the Object class is the superclass of this class and Dynace automatically
    performs this search up the inheratince hierarchy until a matching method
    is found.  This is also true of the New class method defined above.
*/

imeth	object	gDeepDispose, gDispose ()
{
	cNumInstances--;
	return gDispose(super);
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


