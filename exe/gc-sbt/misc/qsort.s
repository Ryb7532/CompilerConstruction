	.globl _append
	.align	4
_append:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rsi,%rax
	cmpq $0,%rdi
	je then62
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rdi
	movq %rax,%rsi
	callq _append
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next63
then62:
next63:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _ltList
	.align	4
_ltList:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rsi,%rax
	cmpq $0,%rax
	je then75
	movq 0(%rax),%rsi
	cmpq %rdi,%rsi
	jl then73
	movq 8(%rax),%rax
	movq %rax,%rsi
	callq _ltList
	jmp next74
then73:
	movq 0(%rax),%rbx
	movq 8(%rax),%rax
	movq %rax,%rsi
	callq _ltList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next74:
	jmp next76
then75:
	movq $0,%rax
next76:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _geList
	.align	4
_geList:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rax
	movq %rsi,%rdi
	cmpq $0,%rdi
	je then96
	movq 0(%rdi),%rsi
	cmpq %rsi,%rax
	je then94
	movq 0(%rdi),%rsi
	cmpq %rsi,%rax
	jl then92
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	jmp next93
then92:
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next93:
	jmp next95
then94:
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next95:
	jmp next97
then96:
	movq $0,%rax
next97:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _qsort
	.align	4
_qsort:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	subq $8,%rsp
	movq %rdi,%rbx
	movq %rbx,%rax
	cmpq $0,%rax
	je then110
	movq 0(%rbx),%rdi
	movq 8(%rbx),%rax
	movq %rax,%rsi
	callq _ltList
	movq %rax,%rdi
	callq _qsort
	movq %rax,%r13
	movq 0(%rbx),%r12
	movq 0(%rbx),%rax
	movq 8(%rbx),%rsi
	movq %rax,%rdi
	callq _geList
	movq %rax,%rdi
	callq _qsort
	movq %r12,%rdi
	movq %rax,%rsi
	callq _cons
	movq %r13,%rdi
	movq %rax,%rsi
	callq _append
	jmp next111
then110:
	movq $0,%rax
next111:
	addq $8,%rsp
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	retq

	.globl _testList
	.align	4
_testList:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rbx
	movq %rbx,%rax
	cmpq $0,%rax
	je then116
	movq %rbx,%rax
	subq $1,%rax
	movq %rax,%rdi
	callq _testList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next117
then116:
	movq $0,%rax
next117:
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
	callq _testList
	movq %rax,%rdi
	callq _qsort
	addq $0,%rsp
	popq %rbp
	retq

