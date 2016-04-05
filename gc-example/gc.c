#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "gc.h"

#define STACK_SIZE 512
#define HEAP_SIZE 1024

static CHUNK* STACK[STACK_SIZE];
static CHUNK** stack_ptr = STACK+STACK_SIZE-1;

static CHUNK FROM_HEAP[HEAP_SIZE];
static CHUNK TO_HEAP[HEAP_SIZE];
static CHUNK* heap = FROM_HEAP;
static CHUNK* heap_ptr = FROM_HEAP; //next free chunk

static uintptr_t stack_size(void)
{
  return (uintptr_t)(STACK+STACK_SIZE-1 - stack_ptr);
}

static void print_chunk(CHUNK* p)
{
  if (p->tag == INT) {
    printf("%lx: CHUNK: tag=%d, data=%u\n",
	   (uintptr_t)p, p->tag, p->data.as_int);
  } else if (p->tag == PTR) {
    printf("%lx: CHUNK: tag=%d, data=%lx\n",
	   (uintptr_t)p, p->tag, (uintptr_t)p->data.as_ptr);
  } else {
    printf("bad chunk tag\n");
    exit (-4);
  }
  return;
}

static void debug(void)
{
  CHUNK* cur = heap;

  printf("heap = %lx\n", (uintptr_t)heap);
  while (cur < heap_ptr) {
    print_chunk(cur);
    cur++;
  }
  printf("heap+heap_ptr\n");
  printf("stack = %lu chunk(s)\n", stack_size());
}

void push(CHUNK* root)
{
  if (stack_size() >= STACK_SIZE) {
    printf("stack overflow\n");
    exit(-1);
  }
  *stack_ptr = root;
  stack_ptr--;
  return;  
}

void pop(void)
{
  if (stack_size() <= 0) {
    printf("stack underflow\n");
    exit (-2);
  }
  stack_ptr++;
  return;
}

CHUNK* new(void)
{
  if (heap_ptr >= heap+HEAP_SIZE) {
    printf("not enough memory\n");
    exit (-3);
  }
  CHUNK* ptr = heap_ptr;
  heap_ptr++;
  return ptr;
}

void copy(CHUNK* from, CHUNK* to)
{
  to->tag = from->tag;
  if (from->tag == INT) {
    to->data.as_int = from->data.as_int;
  } else if (from->tag == PTR) {
    to->data.as_ptr = from->data.as_ptr;
  } else {
    printf("bad chunk tag\n");
    exit (-4);
  }
  return;
}

void gc(void)
{
  CHUNK* scan = (heap==FROM_HEAP) ? TO_HEAP : FROM_HEAP;
  CHUNK* next = scan; //next free chunk
  CHUNK** cur = STACK+STACK_SIZE-1;

  int ncopied_roots = 0;
  int ncopied_blocks = 0;
 
  printf("Starting GC\n");
  
  //Copy the roots
  printf("Copying roots\n");
  while (cur > stack_ptr) {
    copy(*cur, next);
    //Don't forget to update pointer!
    *cur = next;
    ncopied_roots++;
    print_chunk(*cur);
    cur--;
    next++;
  }

  printf("Scanning chunks\n");  
  while (scan < next) {
    if (scan->tag == PTR) {
      copy(scan->data.as_ptr, next);
      ncopied_blocks++;
      //Don't forget to update pointer!
      scan->data.as_ptr = next;
      print_chunk(scan);      
      next++;
    }
    scan++;
  }
  printf("Copied roots = %d, copied blocks = %d\n",
	 ncopied_roots, ncopied_blocks);

  heap = (heap==FROM_HEAP) ? TO_HEAP : FROM_HEAP;
  heap_ptr = next;
  return;
}

int main(void)
{
  CHUNK* c = new();

  printf("\na pointer chunk");
  c->tag = PTR;
  c->data.as_ptr = c;
  debug();

  printf("\nstack is empty\n");
  gc();  
  debug();

  printf("\nan integer chunk\n");
  c = new();
  c->tag = INT;
  c->data.as_int = 23;
  debug();
  push(c);
  printf("\nstack contains c\n");
  gc();
  debug();

  pop();
  printf("\nstack is empty\n");
  gc();
  debug();

  printf("\na pointer chunk\n");
  c = new();
  c->tag = PTR;
  CHUNK* d = new();
  d->tag = INT;
  d->data.as_int = 42;
  c->data.as_ptr = d;
  debug();
  push(c);
  printf("\nstack contains c\n");  
  gc();
  debug();
  printf("\nstack contains c\n");    
  gc();
  debug();

  printf("\nclear stack, allocate 20 blocks, gc\n");    
  pop(); debug();
  for (int i = 0; i < 10; i++) {
    c = new();
    d = new();
    c->tag = PTR;
    c->data.as_ptr = d;
    push(c);
  }
  debug();
  gc();
  debug();
  gc();
  debug();

  printf("\ntry gc'ing A LOT\n");
  for (int i = 0; i < 10000; i++) {
    gc();
  }
  debug();
  
  return 0;
}

  
