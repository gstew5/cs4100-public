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
static CHUNK* heap_ptr = FROM_HEAP;

static void print_chunk(CHUNK* p)
{
  if (p->tag == INT) {
    printf("%lu: CHUNK: tag=%d, data=%u\n",
	   (uintptr_t)p, p->tag, p->data.as_int);
  } else if (p->tag == PTR) {
    printf("%lu: CHUNK: tag=%d, data=%lu\n",
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

  printf("heap = %lu\n", (uintptr_t)heap);
  while (cur < heap_ptr) {
    print_chunk(cur);
    cur++;
  }
  printf("heap+heap_ptr\n");
}

void push(CHUNK* root)
{
  if (stack_ptr <= STACK) {
    printf("stack overflow\n");
    exit(-1);
  }
  *stack_ptr = root;
  stack_ptr--;
  return;  
}

void pop(void)
{
  if (stack_ptr >= STACK+STACK_SIZE) {
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
  CHUNK* next = scan;
  CHUNK** cur = STACK+STACK_SIZE-1;

  printf("Starting GC\n");
  
  //Copy the roots
  printf("Copying roots\n");
  while (cur > stack_ptr) {
    copy(*cur, next);
    print_chunk(*cur);
    cur--;
    next++;
  }

  printf("Scanning chunks\n");  
  while (scan < next) {
    if (scan->tag == PTR) {
      copy(scan->data.as_ptr, next);
      //Don't forget to update pointer!
      scan->data.as_ptr = next;
      print_chunk(scan);      
      next++;
    }
    scan++;
  }

  heap = (heap==FROM_HEAP) ? TO_HEAP : FROM_HEAP;
  heap_ptr = next;
  return;
}

int main(void)
{
  CHUNK* c = new();

  //a pointer chunk
  c->tag = 1;
  c->data.as_ptr = c;
  debug();

  //stack is empty
  gc();  
  debug();

  //an integer chunk
  c = new();
  c->tag = 0;
  c->data.as_int = 23;
  debug();
  push(c);
  //stack contains c
  gc();
  debug();

  pop();
  //stack is empty
  gc();
  debug();

  //a pointer chunk
  c = new();
  c->tag = 1;
  CHUNK* d = new();
  d->tag = 0;
  d->data.as_int = 42;
  c->data.as_ptr = d;
  debug();
  push(c);
  //stack contains c
  gc();
  debug();
  //stack contains c
  gc();
  debug();

  return 0;
}

  
