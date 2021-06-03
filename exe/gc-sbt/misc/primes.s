	.globl _remainder
	.align	4
_remainder:
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

	.globl _range
	.align	4
_range:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rbx
	movq %rsi,%rax
	movq %rbx,%rdi
	addq $1,%rdi
	cmpq %rdi,%rax
	jl then136
	movq %rbx,%rdi
	addq $1,%rdi
	movq %rax,%rsi
	callq _range
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next137
then136:
	movq $0,%rax
next137:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _filter
	.align	4
_filter:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	subq $8,%rsp
	movq %rdi,%r13
	movq %rsi,%r12
	movq %r12,%rax
	cmpq $0,%rax
	je then150
	movq 0(%r12),%rax
	movq %rax,%rdi
	movq %r13,%rsi
	callq _remainder
	cmpq $0,%rax
	je then148
	movq 0(%r12),%rbx
	movq 8(%r12),%rax
	movq %r13,%rdi
	movq %rax,%rsi
	callq _filter
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next149
then148:
	movq 8(%r12),%rax
	movq %r13,%rdi
	movq %rax,%rsi
	callq _filter
next149:
	jmp next151
then150:
	movq $0,%rax
next151:
	addq $8,%rsp
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	retq

	.globl _primesList
	.align	4
_primesList:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rax
	cmpq $0,%rax
	je then159
	movq 0(%rax),%rbx
	movq 0(%rax),%rdi
	movq 8(%rax),%rax
	movq %rax,%rsi
	callq _filter
	movq %rax,%rdi
	callq _primesList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next160
then159:
	movq $0,%rax
next160:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _primes
	.align	4
_primes:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	movq $2,%rdi
	movq %rax,%rsi
	callq _range
	movq %rax,%rdi
	callq _primesList
	addq $0,%rsp
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
	callq _primes
	addq $0,%rsp
	popq %rbp
	retq

