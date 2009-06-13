#include "espresso.h"

VAL evenp(VAL x) {
  return (x%8 == 0);
}

VAL espresso_main() {
  VAL (*p_even)(VAL) = &evenp;
  VAL v_even = (VAL) p_even;

  return filter(&v_even, cons(4,cons(8,nil)));
}
