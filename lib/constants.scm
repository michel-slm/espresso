(define espresso:true  #x9f)
(define espresso:false #x1f)

(define espresso:fxnum-shift 2)

(define espresso:nil #x2f)

(define llvm:int
  (if (zero? (system "uname -m | grep 64 >/dev/null"))
      'i64
      'i32))
