//  Dynace jumpto.s for GNU C compiler (gcc) on 386 machines
	.file	"jumpto.c"
	.version	".01.01"
gcc2_compiled.:
.text
	.align 16
.globl _jumpToMethod
	.type	_jumpToMethod,@function
_jumpToMethod:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%eax
/	call *%eax
/	leave
/	ret
	leave
	leave
	jmp *%eax
.Lfe1:
	.size	_jumpToMethod,.Lfe1-_jumpToMethod
	.ident	"GCC: (GNU) 2.6.3"
