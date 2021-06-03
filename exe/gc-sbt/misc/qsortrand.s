	.globl _append
	.align	4
_append:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rsi,%rax
	cmpq $0,%rdi
	je then178
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rdi
	movq %rax,%rsi
	callq _append
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next179
then178:
next179:
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
	je then191
	movq 0(%rax),%rsi
	cmpq %rdi,%rsi
	jl then189
	movq 8(%rax),%rax
	movq %rax,%rsi
	callq _ltList
	jmp next190
then189:
	movq 0(%rax),%rbx
	movq 8(%rax),%rax
	movq %rax,%rsi
	callq _ltList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next190:
	jmp next192
then191:
	movq $0,%rax
next192:
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
	je then212
	movq 0(%rdi),%rsi
	cmpq %rsi,%rax
	je then210
	movq 0(%rdi),%rsi
	cmpq %rsi,%rax
	jl then208
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	jmp next209
then208:
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next209:
	jmp next211
then210:
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next211:
	jmp next213
then212:
	movq $0,%rax
next213:
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
	movq %rdi,%r12
	movq %r12,%rax
	cmpq $0,%rax
	je then226
	movq 0(%r12),%rdi
	movq 8(%r12),%rax
	movq %rax,%rsi
	callq _ltList
	movq %rax,%rdi
	callq _qsort
	movq %rax,%r13
	movq 0(%r12),%rbx
	movq 0(%r12),%rdi
	movq 8(%r12),%rax
	movq %rax,%rsi
	callq _geList
	movq %rax,%rdi
	callq _qsort
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	movq %r13,%rdi
	movq %rax,%rsi
	callq _append
	jmp next227
then226:
	movq $0,%rax
next227:
	addq $8,%rsp
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	retq

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

	.globl _testList
	.align	4
_testList:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	pushq %r12
	subq $0,%rsp
	movq %rdi,%rbx
	movq %rbx,%rax
	cmpq $0,%rax
	je then237
	callq _rand
	movq %rax,%rdi
	movq $10000,%rsi
	callq _remainder
	movq %rax,%r12
	movq %rbx,%rax
	subq $1,%rax
	movq %rax,%rdi
	callq _testList
	movq %r12,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next238
then237:
	movq $0,%rax
next238:
	addq $0,%rsp
	popq %r12
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

