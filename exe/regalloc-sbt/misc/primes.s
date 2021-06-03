	.globl _remainder
	.align	4
_remainder:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	cqto
	idivq %rsi
	movq %rax,%rdx
	imulq %rsi,%rdx
	movq %rdi,%rax
	subq %rdx,%rax
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
	movq %rbx,%rax
	addq $1,%rax
	cmpq %rax,%rsi
	jl then132
	movq %rbx,%rdi
	addq $1,%rdi
	callq _range
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
	jmp next133
then132:
	movq $0,%rax
next133:
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
	cmpq $0,%r12
	je then139
	movq 0(%r12),%rdi
	movq %r13,%rsi
	callq _remainder
	cmpq $0,%rax
	je then142
	movq 0(%r12),%rbx
	movq 8(%r12),%rsi
	movq %r13,%rdi
	callq _filter
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
	jmp next143
then142:
	movq 8(%r12),%rsi
	movq %r13,%rdi
	callq _filter
next143:
	jmp next140
then139:
	movq $0,%rax
next140:
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
	je then153
	movq 0(%rax),%rbx
	movq 0(%rax),%rdi
	movq 8(%rax),%rsi
	callq _filter
	movq %rax,%rdi
	callq _primesList
	movq %rax,%rsi
	movq %rbx,%rdi
	callq _cons
	jmp next154
then153:
	movq $0,%rax
next154:
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
	movq %rdi,%rsi
	movq $2,%rdi
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
	callq _primes
	addq $0,%rsp
	popq %rbp
	retq

