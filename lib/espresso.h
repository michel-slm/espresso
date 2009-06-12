#ifndef __ESPRESSO_H__
#define __ESPRESSO_H__

#include <stdint.h>

# if __WORDSIZE == 64
#  define VAL int64_t
# else
#  define VAL int32_t
# endif

#define fixnum_mask  3
#define fixnum_tag   0
#define fixnum_shift 2

#define boolean_mask  0x7f
#define boolean_tag   0x1f
#define boolean_shift 7

#define btrue  0x9f
#define bfalse boolean_tag

#define nil 0x2f
#define pair_mask  3
#define pair_tag   1

void display(VAL);
void display_boolean(VAL);
void display_pair(VAL);

#endif
