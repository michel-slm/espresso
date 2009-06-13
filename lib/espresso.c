#include <stdio.h>
#include <stdlib.h>
#include "espresso.h"

#include <assert.h>

typedef struct {
  VAL a;
  VAL d;
} pair;


VAL car(VAL p) {
  VAL *a = (VAL *) (p-1);
  return *a;
}

VAL cdr(VAL p) {
  VAL *d = (VAL *) (p+CDR_OFFSET);
  return *d;
}

VAL cons(VAL a, VAL d) {
  void *v = malloc(2*sizeof(VAL));
  
  VAL *p = (VAL *)v;
  p[0] = a;
  p[1] = d;
  
  return (VAL)p + 1;
}

// predicates
VAL booleanp(VAL val) {
  return ((val & boolean_mask) == boolean_tag);
}

// HOFs
VAL map(VAL proc_addr, VAL ls) {
  VAL (*proc)(VAL) = (VAL (*) (VAL))proc_addr;
  if (ls == nil) return ls;
  else return cons( (*proc)(car(ls)),
                    map(proc_addr, cdr(ls)) );
}

VAL filter(VAL pred_addr, VAL ls) {
  VAL (*pred)(VAL) = (VAL (*) (VAL))pred_addr;
  if (ls == nil) return ls;
  else if ((*pred)(car(ls)) != bfalse)
    return cons(car(ls), filter(pred_addr, cdr(ls)));
  else return filter(pred_addr, cdr(ls));
}


VAL pairp(VAL val) {
  return ((val & pair_mask) == pair_tag);
}

void display(VAL val) {
  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%d", (val >> 2));
  } else if (pairp(val)) {
    display_pair(val);
  } else if (val == nil) {
    printf("()");
  } else if (booleanp(val)) {
    display_boolean(val);
  } else {
    printf("Error: type of %d unknown\n", val);
  }
}

void display_boolean(VAL val) {
  printf("#%c", (val >> boolean_shift == 1) ? 't' : 'f');
}

void display_pair(VAL val) {
  printf("(");
  display(car(val));
  VAL x = cdr(val);
  while (pairp(x)) {
    printf(" ");
    display(car(x));
    x = cdr(x);
  }
  if (x != nil) {
    printf(" . ");
    display(x);
  }
  printf(")");
}

VAL main() {
  display(espresso_main());  
  printf("\n");
}
