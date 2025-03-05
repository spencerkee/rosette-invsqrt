#lang rosette/safe
(require math/flonum)

; Fast inverse square root without Newton's method
(define (fisr number)
  (bit-field->flonum (- 6910387280431496141 (arithmetic-shift (flonum->bit-field number) -1)))
)

(define (isr number)
  (/ 1 (sqrt number))
)

(displayln "The 1/sqrt(4.0) is:")
(fisr 4.0) ; Returns 0.4785533905932738, the real value is 0.5

; TODO (assert (fl<= my_isr 1.0))?
; Spec that verifies a function finds an inverse square
; root (1/sqrt(num)) within a certain error bound.
(define (check-fisr impl num)                ; Assuming that
  (assume (> num 0.0))                    ; num is greater than 0
  (define actual_isr (/ 1 (sqrt num)))       ; let actual_isr = 1/sqrt(num)
  (define my_isr (impl num))                 ; let my_isr = impl(num)
  (define diff (abs (- actual_isr my_isr)))  ; let diff = abs(actual_isr-my_isr)
  (define error (/ diff actual_isr))         ; let error = diff/actual_isr 
  (assert (<= error 0.05))                   ; we require that error is less than some val
)

(display "Checking fisr's spec against a concrete legal input\n")
; (check-fisr fisr 1.0) ; This sucessfully triggers the assert

; Input real symbolic constant
(define-symbolic input_realsc real?)
(define cex (verify (check-fisr fisr input_realsc)))
(define c_input_realsc (evaluate input_realsc cex))
(printf "Counterexample=~a\nfisr=~a\nactual=~a\nerror=~a\n"
    c_input_realsc
    (fisr c_input_realsc)
    (isr c_input_realsc)
    (/ (abs (- (isr c_input_realsc) (fisr c_input_realsc))) (isr c_input_realsc))
)
(check-fisr fisr c_input_realsc) ; Why doesn't this trigger the assertion?

; TODO For future reference, rosette/safe does not include flonums. Or sqrt for that matter.
; So I need to reimplement sqrt myself, and flonum->bitfield and bitfield->flonum (except that
; flonum will really be a bit representation of the float, so also have to write real->flonum
; and flonum->real. Also everything has to be finitized like in the bvsqrt function here:
; https://docs.racket-lang.org/rosette-guide/ch_essentials.html#(part._.Symbolic_.Evaluation)
