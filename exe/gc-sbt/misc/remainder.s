	.globl _test
	.align	4
_test:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	cqto
	idivq %rsi
	imulq %rsi,%rax
	subq %rax,%rdi
	movq %rdi,%rax
	addq $0,%rsp
	popq %rbp
	retq

