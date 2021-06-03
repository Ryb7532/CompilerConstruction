	.globl _test
	.align	4
_test:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	cqto
	idivq %rsi
	movq %rax,%rdx
	imulq %rsi,%rdx
	movq %rdi,%rax
	subq %rdx,%rax
	addq $0,%rsp
	popq %rbp
	retq

