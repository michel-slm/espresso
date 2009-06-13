#include "espresso.h"

VAL map(VAL, VAL);

VAL add1(VAL n) {
  return n+4;
}

VAL espresso_main() {
  VAL (*p_add1)(VAL) = &add1;
  VAL v_add1 = (VAL) p_add1;
  //return map(v_add1, nil);
  return map(v_add1, cons(4, cons(8, cons(12, nil))));
}
