#include <assert.h>
#include "espresso.h"

VAL apply_twice1(VAL proc, VAL v) {
  VAL (*realproc)(VAL) = (VAL (*) (VAL))proc;
  return (*realproc)((*realproc)(v));
}

VAL apply_twice2(VAL (*proc) (VAL), VAL v) {
  return (*proc)((*proc)(v));
}

VAL test_fp_cast(VAL proc_v, VAL (*proc_p) (VAL)) {
  VAL (*proc_p2) (VAL) = (VAL (*) (VAL)) proc_v;
  assert(proc_p == proc_p2);
  return btrue;
}

VAL add1(VAL n) {
  return n+4;
}

VAL espresso_main() {
  VAL (*p_add1)(VAL) = &add1;
  VAL v_add1 = (VAL) p_add1;
  //return test_fp_cast(v_add1, p_add1);
  return apply_twice1(v_add1, 160);
  //return apply_twice2(p_add1, 160);
}
