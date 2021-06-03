	.globl _test
	.align	4
_test:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	movq %rsi,%rdi
	movq %rdx,%rsi
	subq %rdi,%rax
	imulq %rsi,%rax
	addq $0,%rsp
	popq %rbp
	retq

