#include<stdio.h>
#include<stdlib.h>
extern struct cons *test(long x0, long x1, long x2, long x3, long x4, long x5);
struct cons {
  long head;
  struct cons *tail;
};

#define N 10000
struct cons heap[N];
int i = 0;

struct cons *cons(long head, struct cons *tail) {
  if (i >= N) {
    printf("Heap is full!\n");
    exit(-1);
  }
  struct cons *r = &heap[i];
  r->head = head;
  r->tail = tail;
  i++;
  return r;
}
  
int main(int argc, char *argv[])  {
  long a[6] = {0,0,0,0,0,0};

  for (int i = 1; i< argc; i++) 
    a[i-1] = atoi(argv[i]);
  
  struct cons *l = test(a[0], a[1], a[2], a[3], a[4], a[5]);
  
  if (heap <= l && l <= &heap[N-1])
    for (;l != NULL; l = l-> tail)
      printf("%ld,", l->head);
  else
    printf("result: %ld", (long) l);
  printf("\n");
}





