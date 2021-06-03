	.globl _fact
	.align	4
_fact:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rbx
	cmpq $0,%rbx
	je then3
	movq %rbx,%rdi
	subq $1,%rdi
	callq _fact
	movq %rax,%rdi
	movq %rbx,%rax
	imulq %rdi,%rax
	jmp next4
then3:
	movq $1,%rax
next4:
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
	callq _fact
	addq $0,%rsp
	popq %rbp
	retq

