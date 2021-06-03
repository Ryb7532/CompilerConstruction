	.globl _insert
	.align	4
_insert:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	cmpq $0,%rsi
	je then29
	movq 0(%rsi),%rax
	cmpq %rax,%rdi
	jl then33
	movq 0(%rsi),%rbx
	movq 8(%rsi),%rsi
	callq _insert
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
	jmp next34
then33:
	callq _cons
next34:
	jmp next30
then29:
	movq $0,%rsi
	callq _cons
next30:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _sort
	.align	4
_sort:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	cmpq $0,%rdi
	je then42
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rdi
	callq _sort
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _insert
	jmp next43
then42:
	movq $0,%rax
next43:
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
	movq $3,%rdi
	movq $0,%rsi
	callq _cons
	movq %rax,%rsi
	movq $1,%rdi
	callq _cons
	movq %rax,%rsi
	movq $5,%rdi
	callq _cons
	movq %rax,%rsi
	movq $2,%rdi
	callq _cons
	movq %rax,%rsi
	movq $4,%rdi
	callq _cons
	movq %rax,%rdi
	callq _sort
	addq $0,%rsp
	popq %rbp
	retq

