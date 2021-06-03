	.globl __gc
	.align	4, 0x90
__gc:
	pushq %rbp
	movq %rsp,%rbp
        
        pushq %rbx
        pushq %r12
        pushq %r13
        pushq %r14
        pushq %r15
        pushq %rax
        callq _gc
        popq %rax
        popq %r15
        popq %r14
        popq %r13
        popq %r12
        popq %rbx
        
        popq %rbp
	retq

