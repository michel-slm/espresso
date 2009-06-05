#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "espresso.h"

#include <assert.h>

typedef struct {
  int32_t a;
  int32_t d;
} pair;


int32_t car(int32_t p) {
  int32_t *a = (int32_t *) (p-1);
  return *a;
}

int32_t cdr(int32_t p) {
  int32_t *d = (int32_t *) (p+3);
  return *d;
}

int32_t cons(int32_t a, int32_t d) {
  void *v = malloc(2*sizeof(int32_t));
  
  int32_t *p = (int32_t *)v;
  p[0] = a;
  p[1] = d;
  
  return (int32_t)p + 1;
}

// predicates
int32_t booleanp(int32_t val) {
  return ((val & boolean_mask) == boolean_tag);
}

int32_t pairp(int32_t val) {
  return ((val & pair_mask) == pair_tag);
}

void display(int32_t val) {
  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%d", (val >> 2));
  } else if (pairp(val)) {
    display_pair(val);
  } else if (val == empty_list) {
    printf("()");
  } else if (booleanp(val)) {
    display_boolean(val);
  } else {
    printf("Error: type of %d unknown\n", val);
  }
}

void display_boolean(int32_t val) {
  printf("#%c", (val >> boolean_shift == 1) ? 't' : 'f');
}

void display_pair(int32_t val) {
  printf("(");
  display(car(val));
  int32_t x = cdr(val);
  while (pairp(x)) {
    printf(" ");
    display(car(x));
    x = cdr(x);
  }
  if (x != empty_list) {
    printf(" . ");
    display(x);
  }
  printf(")");
}

int32_t main() {
  int32_t x = 42 << 2;
  int32_t y = 7 << 2;
  display(x);
  printf("\n");
  int32_t p = cons(x, cons(y, cons(btrue, cons(bfalse, empty_list))));
  display(p);
  printf("\n");
  display(espresso_main());  
  printf("\n");
}
