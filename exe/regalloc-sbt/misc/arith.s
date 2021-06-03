	.globl _test
	.align	4
_test:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	subq %rsi,%rax
	imulq %rdx,%rax
	addq $0,%rsp
	popq %rbp
	retq

