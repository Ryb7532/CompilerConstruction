	.globl _increase
	.align	4
_increase:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	movq %rsi,%rdx
	movq %rax,%rdi
	addq $1,%rdi
	movq %rax,%rsi
	addq $1,%rsi
	imulq %rsi,%rdi
	movq %rdx,%rsi
	cmpq %rdi,%rsi
	jl then15
	addq $1,%rax
	jmp next16
then15:
next16:
	addq $0,%rsp
	popq %rbp
	retq

	.globl _introot
	.align	4
_introot:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rbx
	movq %rbx,%rax
	cmpq $0,%rax
	je then22
	movq %rbx,%rax
	cqto
	movq $4,%rdi
	idivq %rdi
	movq %rax,%rdi
	callq _introot
	movq %rax,%rdi
	movq $2,%rax
	imulq %rdi,%rax
	movq %rax,%rdi
	movq %rbx,%rsi
	callq _increase
	jmp next23
then22:
	movq $0,%rax
next23:
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
	callq _introot
	addq $0,%rsp
	popq %rbp
	retq

