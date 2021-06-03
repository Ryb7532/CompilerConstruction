SRC=arith.s fact.s sort.s qsort.s primes.s introot.s remainder.s
OBJ = $(SRC:%.s=%.o)
TARGET = $(SRC:%.s=%) 

all: $(TARGET)

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

primes.o: primes.s

primes: primes.o main.o
	gcc $^ -o $@

introot.o: introot.s

introot: introot.o main.o
	gcc $^ -o $@

clean:
	-rm main.o
	-rm $(TARGET)
	-rm $(OBJ)
	-rm $(SRC)

test:
	@echo "arith 1 2 3 should be -3"
	./arith 1 2 3
	@echo "fact 4 should be 24"
	./fact 4
	@echo "sort should be 1,2,3,4,5"
	./sort
	@echo "qsort 5 should be 1,2,3,4,5"
	./qsort 5
	@echo "primes 20 should be 2,3,5,7,9,11,13,17,19"
	./primes 20
	@echo "introot 25 should be 5"
	./introot 25
	@echo "introot 28 should be 6"
	./introot 38
	@echo "remainder 11 3 should be 2"
	./remainder 11 3
	@echo "remainder 12 3 should be 0"
	./remainder 12 3

