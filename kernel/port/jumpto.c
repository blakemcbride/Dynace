 
void	_jumpToMethod(void (*function) (/* ??? */))
{

/* 	pop_this_stack_frame;	*/

/* 	pop previous (generics) stack frame  */

 	(*function)();	/*  must be changed to jump instruction   */
 }
 
