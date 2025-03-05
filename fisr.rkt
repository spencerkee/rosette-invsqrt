#lang rosette

(require math/flonum)

(define (fisr number)
  (bit-field->flonum (- 6910387280431496141 (arithmetic-shift (flonum->bit-field number) -1)))
)

(fisr 4.0)
