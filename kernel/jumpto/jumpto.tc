;	Dynace jumpto function for Turbo C++ 1.00 - large memory model
;
	ifndef	??version
?debug	macro
	endm
$comm	macro	name,dist,size,count
	comm	dist name:BYTE:count*size
	endm
	else
$comm	macro	name,dist,size,count
	comm	dist name[size]:BYTE:count
	endm
	endif
	?debug	S "jumpto.c"
	?debug	C E99C4CFF14086A756D70746F2E63
	?debug	C E95639FE14056F69632E68
	?debug	C E99B49FF140A6F69636F6E6669672E68
	?debug	C E90008A41416663A5C54435C494E434C5544455C7374646465662E+
	?debug	C 68
	?debug	C E90008A41416663A5C54435C494E434C5544455C7374646172672E+
	?debug	C 68
	?debug	C E95539FE140A67656E65726963732E68
	?debug	C E95539FE14076572726F722E68
	?debug	C E95539FE140A65786373746174652E68
	?debug	C E90008A41416663A5C54435C494E434C5544455C7365746A6D702E+
	?debug	C 68
	?debug	C E90B4EFF140970726F636573732E68
	?debug	C E95539FE14076572726F722E68
	?debug	C E90008A41414663A5C54435C494E434C5544455C74696D652E68
JUMPTO_TEXT	segment byte public 'CODE'
JUMPTO_TEXT	ends
DGROUP	group	_DATA,_BSS
	assume	cs:JUMPTO_TEXT,ds:DGROUP
_DATA	segment word public 'DATA'
d@	label	byte
d@w	label	word
_DATA	ends
_BSS	segment word public 'BSS'
b@	label	byte
b@w	label	word
_BSS	ends
JUMPTO_TEXT	segment byte public 'CODE'
   ;	
   ;	void	_jumpToMethod(func)
   ;	
	assume	cs:JUMPTO_TEXT
__jumpToMethod	proc	far
	push	bp
	mov	bp,sp
   ;	
   ;	void	(*func)();
   ;	 {
   ;	
   ;	/* 	pop_this_stack_frame;	*/
   ;	
   ;	/* 	pop previous (generics) stack frame  */
   ;	
   ;	 	(*func)();	/*  must be changed to jump instruction   */
   ;	
;	call	dword ptr [bp+6]
   ;	
   ;	 }
   ;	
;	pop	bp
;	ret	
;
;
;	NEW CODE:
;
;	save jump address
;
	mov	ax, word ptr [bp+6]
	mov	dx, word ptr [bp+8]
;
;	restore current state
;
	pop	bp
;
;	restore previous state
;
	add	sp,8
	pop	bp
;
;	perform jump
;
	push	dx
	push	ax
	ret
__jumpToMethod	endp
	?debug	C E9
JUMPTO_TEXT	ends
_DATA	segment word public 'DATA'
s@	label	byte
_DATA	ends
JUMPTO_TEXT	segment byte public 'CODE'
JUMPTO_TEXT	ends
	public	__jumpToMethod
	end
