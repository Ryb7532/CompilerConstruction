	.globl _increase
	.align	4
_increase:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	movq %rax,%rdx
	addq $1,%rdx
	movq %rax,%rdi
	addq $1,%rdi
	imulq %rdi,%rdx
	cmpq %rdx,%rsi
	jl then11
	addq $1,%rax
	jmp next12
then11:
next12:
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
	cmpq $0,%rbx
	je then18
	movq %rbx,%rax
	cqto
	movq $4,%rdi
	idivq %rdi
	movq %rax,%rdi
	callq _introot
	movq $2,%rdi
	imulq %rax,%rdi
	movq %rbx,%rsi
	callq _increase
	jmp next19
then18:
	movq $0,%rax
next19:
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
	callq _introot
	addq $0,%rsp
	popq %rbp
	retq

