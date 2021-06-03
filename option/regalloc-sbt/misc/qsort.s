	.globl _append
	.align	4
_append:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rsi,%rax
	cmpq $0,%rdi
	je then58
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rdi
	movq %rax,%rsi
	callq _append
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
	jmp next59
then58:
next59:
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
	cmpq $0,%rsi
	je then65
	movq 0(%rsi),%rax
	cmpq %rdi,%rax
	jl then68
	movq 8(%rsi),%rsi
	callq _ltList
	jmp next69
then68:
	movq 0(%rsi),%rbx
	movq 8(%rsi),%rsi
	callq _ltList
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
next69:
	jmp next66
then65:
	movq $0,%rax
next66:
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
	cmpq $0,%rsi
	je then78
	movq 0(%rsi),%rax
	cmpq %rax,%rdi
	je then81
	movq 0(%rsi),%rax
	cmpq %rax,%rdi
	jl then89
	movq 8(%rsi),%rsi
	callq _geList
	jmp next90
then89:
	movq 0(%rsi),%rbx
	movq 8(%rsi),%rsi
	callq _geList
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
next90:
	jmp next82
then81:
	movq 0(%rsi),%rbx
	movq 8(%rsi),%rsi
	callq _geList
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
next82:
	jmp next79
then78:
	movq $0,%rax
next79:
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
	movq %rdi,%r13
	cmpq $0,%r13
	je then99
	movq 0(%r13),%rdi
	movq 8(%r13),%rsi
	callq _ltList
	movq %rax,%rdi
	callq _qsort
	movq %rax,%rbx
	movq 0(%r13),%r12
	movq 0(%r13),%rdi
	movq 8(%r13),%rsi
	callq _geList
	movq %rax,%rdi
	callq _qsort
	movq %rax,%rsi
	movq %r12,%rdi
	callq _cons
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _append
	jmp next100
then99:
	movq $0,%rax
next100:
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
	cmpq $0,%rbx
	je then113
	movq %rbx,%rdi
	subq $1,%rdi
	callq _testList
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
	jmp next114
then113:
	movq $0,%rax
next114:
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
	callq _testList
	movq %rax,%rdi
	callq _qsort
	addq $0,%rsp
	popq %rbp
	retq

