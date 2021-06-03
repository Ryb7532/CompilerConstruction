#include<stdio.h>
#include<stdlib.h>
extern struct cons *test(long x0, long x1, long x2, long x3, long x4, long x5);
extern void _gc();

struct cons {
  long head;
  struct cons *tail;
};

void mark_cons(struct cons *p) {
  p->tail = (struct cons *) ((unsigned long)(p->tail) | 1);
  return;
}

void unmark_cons(struct cons *p) {
  if (p->tail == (struct cons*)0x1) p->tail = NULL;
  else p->tail = (struct cons*)((unsigned long)(p->tail) - 1);
}

int marked_cons(struct cons * p) {
  if (p->tail == NULL) return 0;
  else return (int)((unsigned long)(p->tail) & 0x1);
}



#define N 1000
struct cons heap[N];

struct cons *free_list = NULL;

void sweep() {
  struct cons *p = heap;
  while (p < heap+N) {
    if (marked_cons(p)) unmark_cons(p);
    else {
      p->tail = free_list;
      free_list = p;
    }
    p += &heap[1] - heap;
  }
}

void mark(struct cons *l) {
  if (l->tail == NULL) l->tail = (struct cons*)0x1;
  else if (!marked_cons(l)) {
    struct cons* lt = l->tail;
    mark_cons(l);
    mark(lt);
  }
}

void *stack_limit_high;

void gc() {
  struct cons *dummy;

  // dummy から stack_limit_high までのスタック上の値で
  // 配列heap内のstruct consへのポインタと考えられる値をmarkする
  for (struct cons **p = &dummy; p < (struct cons **) stack_limit_high; p++)
    if(heap <= *p && *p < heap+N)
      if (((long) *p - (long) heap) % (&heap[1] - heap) == 0)
        mark(*p);
  sweep();
}

struct cons *cons(long head, struct cons *tail) {
  if (free_list == NULL) {
    printf("GC.\n");
    // Call saveレジスタをスタックに保存するためアセンブリ言語で
    // 書かれた関数を呼ぶ．_gc()がgc()を呼ぶ．
    _gc();
    if (free_list == NULL) {
      printf("Cannot obtain free cons cells!\n");
      exit(-1);
    }
  }
  struct cons *r = free_list;
  free_list = free_list->tail;
  r->head = head;
  r->tail = tail;
  return r;
}

int main(int argc, char *argv[])  {
  long a[6] = {0,0,0,0,0,0};

  for (int i = 1; i< argc; i++)
    a[i-1] = atoi(argv[i]);

  struct cons dummy;
  stack_limit_high = &dummy;

  for (int i = N-1; i >= 0; i--) {
    heap[i].tail = free_list;
    free_list = &heap[i];
  }

  struct cons *l = test(a[0], a[1], a[2], a[3], a[4], a[5]);
  if (heap <= l && l <= &heap[N-1])
    for (;l != NULL; l = l-> tail)
      printf("%ld,", l->head);
  else
    printf("result: %ld", (long) l);
  printf("\n");
}





