#ifndef __GC_H
#define __GC_H

enum TAG {
  INT,
  PTR
};

typedef struct chunk {
  enum TAG tag;
  union {
    unsigned int as_int;
    struct chunk* as_ptr;
  } data;
} CHUNK;

void push(CHUNK* root);
void pop(void);
CHUNK* new(void);
void gc(void);

#endif 
