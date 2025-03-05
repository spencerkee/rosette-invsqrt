#lang rosette

(require math/flonum)

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))
; int32 takes as input an integer literal and returns
; the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

; integer->integer-bytes takes in n, size-n, signed? [big-endian?, dest-bstr, start]
; Converts the exact integer n to a machine-format number encoded in a byte string
; of length size-n, which must be 1, 2, 4, or 8. If signed? is true, then the number
; is encoded as two’s complement, otherwise it is encoded as an unsigned bit stream.
; If big-endian? is true, then the most significant eight bits of the number are
; encoded in the first byte of the resulting byte string, otherwise the least-
; significant bits are encoded in the first byte, and so on.
 
; (define (bit-field->float32 x)
;  (floating-point-bytes->real (integer->integer-bytes x 4 #f #f) #f))

; (integer->integer-bytes 7 4 #f #f)

; (define (bit-field->float32 x)
;   (floating-point-bytes->real (integer->integer-bytes x 4 #f #f) #f))

; (bit-field->float32 5)
; (integer->integer-bytes 2 4 #f #f)

; (flonum->bit-field 5.0)
; (bvlshr (integer->bitvector (flonum->bit-field 5.0) 64) (bv 1 (bitvector 64)))
; (integer-bytes->integer (real->floating-point-bytes 5.0 4) #f)

; (integer->bitvector 4617315517961601024 (bitvector 64))

; (bvlshr (integer->bitvector 4617315517961601024 (bitvector 64)) (bv 1 64))

; Convert 5.0 to an integer that represents a bitfield. Shift it right one, then convert it back to an integer
; (- #x5f3759df (bitvector->integer (bvlshr (integer->bitvector (flonum->bit-field 5.0) (bitvector 64)) (bv 1 64))))

(define (intbv->flonum int_bv)
  (bit-field->flonum int_bv)
  ;(bit-field->flonum (integer->bitvector int_bv (bitvector 64)))
)

; (print #x5f3759df)
(define (flonum->bv number)
  (integer->bitvector (flonum->bit-field number) (bitvector 64))
)

; TODO use (arithmetic-shift n m) -1
(define (fisr number)
  (let ([threehalfs 1.5])
    ; (let ([i (- #x43e6a09e667f3bcd (bitvector->integer (bvlshr (flonum->bv number) (bv 1 64))))])
    (let ([shifted (arithmetic-shift (flonum->bit-field number) -1)])
      (displayln shifted)
      (let ([i (- 6910387280431496141 shifted)])
        (displayln i)
        ; (displayln "\n")
        (displayln (bit-field->flonum i))
        (let ([y (bit-field->flonum i)])
          (let ([answer (* y (- threehalfs (* (* number 0.5) y y)))])
            answer
            ; (flonum->bit-field y)
          )
        )
      )
    )
  )
)


(define (fisr2 number)
  (let ([shifted (arithmetic-shift (flonum->bit-field number) -1)])
    (let ([i (- 6910387280431496141 shifted)])
      (bit-field->flonum i)
    )
  )
)

(define (fisr3 number)
  (bit-field->flonum (- 6910387280431496141 (arithmetic-shift (flonum->bit-field number) -1)))
)

;#x43e6a09e667f3bcd

; (display "1/sqrt(5.0) equals\n")
(fisr 4.0)
(fisr2 4.0)
(fisr3 4.0)
; (flonum->bv 1.0)
; (flonum->bit-field (sqrt (expt 2 1023)))

; (single-flonum-available?)
; (system-type 'word)
; (single-flonum-available?)
