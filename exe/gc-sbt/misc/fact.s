	.globl _fact
	.align	4
_fact:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rbx
	movq %rbx,%rax
	cmpq $0,%rax
	je then6
	movq %rbx,%rax
	subq $1,%rax
	movq %rax,%rdi
	callq _fact
	movq %rbx,%rdi
	imulq %rax,%rdi
	movq %rdi,%rax
	jmp next7
then6:
	movq $1,%rax
next7:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _test
	.align	4
_test:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	movq %rax,%rdi
	callq _fact
	addq $0,%rsp
	popq %rbp
	retq

