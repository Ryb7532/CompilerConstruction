 #include<stdio.h>
#include<stdlib.h>
extern struct cons *test(long x0, long x1, long x2, long x3, long x4, long x5);
extern void _gc();

struct cons {
  long head;
  struct cons *tail;
  char mark;
};

#define N 1000
struct cons heap[N];

struct cons *free_list = NULL;

void sweep () {
  struct cons *p = heap;
  while (p < heap+N) {
    if (p->mark == 't') p->mark = 'f';
    else if (p->tail != NULL) {
      p->tail = free_list;
      free_list = p;
    }
    p += &heap[1] - heap;
  }
  return ;
}

void mark(struct cons *l) {
  if (l->mark != 't') {
    l->mark = 't';
    if (l->tail == NULL) return ;
    return mark(l->tail);
  }
  return ;
}

void *stack_limit_high;

void gc () {
  struct cons *dummy;

  // dummy から stack_limit_high までのスタック上の値で
  // 配列heap内のstruct consへのポインタと考えられる値をmarkする
  for (struct cons **p = &dummy; p < (struct cons **) stack_limit_high; p++)
    if(heap <= *p && *p < &heap[N])
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





