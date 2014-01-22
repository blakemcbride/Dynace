
/*  This routine works with WATCOM C/C++32 version 9.5 & 10.0
    (See other jumpto.w* files for other configurations)
    It uses the stack parameter passing convention (used by NLMs) and
    no stack checking
	compile with:
		wcl386 -4s -c -I../include -zp2 -zq -oilrt -s file.c
*/



#ifdef	__cplusplus
extern "C"  {
#endif



typedef int     (*fun)();

void DB(void);
void NDB(void);

#pragma aux DB = "add esp,14H" \
                    "pop eax" \
                    "pop ebp" \
                    "add esp,0CH" \
                    "jmp eax"

#pragma aux NDB = "add esp,8H" \
                    "jmp eax"


void    _jumpToMethod( fun f )
{

/*      pop_this_stack_frame;   */

/*      pop previous (generics) stack frame  */

   NDB();

/*	(*f)();  */

}


#ifdef	__cplusplus
}
#endif
