	.globl _insert
	.align	4
_insert:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rsi,%rax
	cmpq $0,%rax
	je then39
	movq 0(%rax),%rsi
	cmpq %rsi,%rdi
	jl then37
	movq 0(%rax),%rbx
	movq 8(%rax),%rax
	movq %rax,%rsi
	callq _insert
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next38
then37:
	movq %rax,%rsi
	callq _cons
next38:
	jmp next40
then39:
	movq $0,%rsi
	callq _cons
next40:
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
	movq %rdi,%rax
	cmpq $0,%rax
	je then46
	movq 0(%rax),%rbx
	movq 8(%rax),%rax
	movq %rax,%rdi
	callq _sort
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _insert
	jmp next47
then46:
	movq $0,%rax
next47:
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
	movq $1,%rdi
	movq %rax,%rsi
	callq _cons
	movq $5,%rdi
	movq %rax,%rsi
	callq _cons
	movq $2,%rdi
	movq %rax,%rsi
	callq _cons
	movq $4,%rdi
	movq %rax,%rsi
	callq _cons
	movq %rax,%rdi
	callq _sort
	addq $0,%rsp
	popq %rbp
	retq

