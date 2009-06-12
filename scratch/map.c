#include "espresso.h"

int map(int (*proc)(int), int ls) {
  if (ls == nil) return ls;
  else return cons( (*proc)(car(ls)), 
		    map(proc, cdr(ls)) );
}

int add1(int n) {
  return n+4;
}

int espresso_main() {
  return map(&add1, cons(4, cons(8, cons(12, nil))));
}
