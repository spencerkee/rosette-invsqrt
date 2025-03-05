#lang rosette

(require math/flonum)

(define (fisr number)
  (bit-field->flonum (- 6910387280431496141 (arithmetic-shift (flonum->bit-field number) -1)))
)

(displayln "The 1/sqrt(4.0) is:")
(fisr 4.0)

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))
; int32 takes as input an integer literal and returns
; the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

; Define the spec check-mid.
(define (check-mid impl lo hi)     ; Assuming that
  (assume (bvsle (int32 0) lo))    ; 0 ≤ lo and
  (assume (bvsle lo hi))           ; lo ≤ hi,
  (define mi (impl lo hi))         ; and letting mi = impl(lo, hi) and
  (define diff                     ; diff = (hi - mi) - (mi - lo),
    (bvsub (bvsub hi mi)
           (bvsub mi lo)))         ; we require that
  (assert (bvsle lo mi))           ; lo ≤ mi,
  (assert (bvsle mi hi))           ; mi ≤ hi,
  (assert (bvsle (int32 0) diff))  ; 0 ≤ diff, and
  (assert (bvsle diff (int32 1)))) ; diff ≤ 1.

; TODO assert that the inverse square root is less than 1?
(define (check-fisr impl num) ; Assuming that
  (assume (fl>= num 0.0))  ; num is greater than 0
  (define actual_isr (/ 1 (sqrt num))) ; actual_isr = 1/sqrt(num)
  (define my_isr (impl num))
  (assert (fl<= my_isr 1.0))
  (define diff (abs (- actual_isr my_isr)))
  (define percent_error (/ diff actual_isr))
  (assert (fl<= percent_error 0.05))
)

(display "Checking bvmid against it's spec against a concrete legal input\n")
(check-fisr fisr 4.0)
