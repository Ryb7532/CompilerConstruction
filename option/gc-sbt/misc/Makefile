SRC=append.s arith.s fact.s sort.s qsort.s primes.s introot.s remainder.s qsortrand.s
OBJ = $(SRC:%.s=%.o)
TARGET = $(SRC:%.s=%) qsort-gc qsort-gcbit primes-gc primes-gcbit qsortrand-gc qsortrand-gcbit

all: $(TARGET)

append.o: append.s

append: append.o main.o
	gcc $^ -o $@

arith.o: arith.s

arith: arith.o main.o
	gcc $^ -o $@

remainder.o: remainder.s

remainder: remainder.o main.o
	gcc $^ -o $@

fact.o: fact.s

fact: fact.o main.o
	gcc $^ -o $@

sort.o: sort.s

sort: sort.o main.o
	gcc $^ -o $@

qsort.o: qsort.s

qsort: qsort.o main.o
	gcc $^ -o $@

qsortrand: qsortrand.o main.o
	gcc $^ -o $@

primes.o: primes.s

primes: primes.o main.o
	gcc $^ -o $@

introot.o: introot.s

introot: introot.o main.o
	gcc $^ -o $@

gc.o: gc.s

qsort-gc: qsort.o maingc.o gc.o
	gcc $^ -o $@

primes-gc: primes.o maingc.o gc.o
	gcc $^ -o $@

qsortrand.o: qsortrand.s

qsortrand-gc: qsortrand.o maingc.o gc.o
	gcc $^ -o $@


qsort-gcbit: qsort.o maingcbit.o gc.o
	gcc $^ -o $@

primes-gcbit: primes.o maingcbit.o gc.o
	gcc $^ -o $@

qsortrand-gcbit: qsortrand.o maingcbit.o gc.o
	gcc $^ -o $@


clean:
	-rm main.o
	-rm maingc.o
	-rm maingcbit.o
	-rm gc.o
	-rm $(TARGET)
	-rm $(OBJ)
	-rm $(SRC)

test:
	@echo "append (1,2,3) (4,5) should be 1,2,3,4,5"
	./append
	@echo "arith 1 2 3 should be -3"
	./arith 1 2 3
	@echo "fact 4 should be 24"
	./fact 4
	@echo "sort should be 1,2,3,4,5"
	./sort
	@echo "qsort 5 should be 1,2,3,4,5"
	./qsort 5
	@echo "primes 20 should be 2,3,5,7,11,13,17,19"
	./primes 20
	@echo "introot 25 should be 5"
	./introot 25
	@echo "introot 38 should be 6"
	./introot 38
	@echo "remainder 11 3 should be 2"
	./remainder 11 3
	@echo "remainder 12 3 should be 0"
	./remainder 12 3

	@echo "qsort-gc 44 should be OK"
	./qsort-gc 44
	@echo "qsort-gcbit 44 should be OK"
	./qsort-gcbit 44
	@echo "primes-gc 550 should be OK"
	./primes-gc 550
	@echo "primes-gcbit 550 should be OK"
	./primes-gcbit 550
	@echo "qsortrand-gc 100 should be OK"
	./qsortrand-gc 100
	@echo "qsortrand-gcbit 100 should be OK"
	./qsortrand-gcbit 100
