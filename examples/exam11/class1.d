

/*  This class definition builds on the previous example with the addition
    of three instance variable declarations.  Notice how the declaration
    looks similar to a normal C structure definition.  Remember that
    instance variables are those variables which will be uniquely allocated
    for each instance of the class.

    Notice the naming convention used.  The leading i (which identifies
    the variable as an instance variable) and the following upper case
    letter make up the normal instance variable names.  The nameing of
    instance variables is just a convention and is not enforced by
    Dynace.  You may use another.

    Notice that the instance variable iData has no type declaration.
    When declaring instance (or class) variables, if no type is defined
    Dynace defaults the type to 'object'.
*/

defclass  Class1  {
	char	iName[30];
	int	iCode;
	iData;
};

