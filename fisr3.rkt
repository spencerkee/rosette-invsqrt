#lang rosette/safe
(require math/flonum)

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))
; int32 takes as input an integer literal and returns
; the corresponding 32-bit bitvector value.
; To do the inverse use bitvector->integer
(define (int32 i)
  (bv i int32?))

(define (flobit->real flobit)
  (*
    (if
      (bveq
        (bit 31 flobit)
        (bv #b1 1)
      )
      -1
      1
    )
    (expt
      2
      (-
        (bitvector->natural
          (bvlshr
            (bvshl
              flobit
              (bv #b1 32)
            )
            (bv 24 32)
          )
        )
        127
      )
    )
    (/
      (bitvector->natural
        (bvadd
          (bv 8388608 32)
          (bvlshr
            (bvshl
              flobit
              (bv 9 32)
            )
            (bv 9 32)
          )
        )
      )
      (expt 2 23)
    )
  )
)

; (bitvector->natural (bit 31 (bv #b00111110001000000000000000000000 32)))

(flobit->real (bv #b00111110001000000000000000000000 32))

; bvlshr is right, bvshl is left
; (bvadd (bv 8388608 32) (bvlshr (bvshl (bv #b10111110010000000000000000000000 32) (bv 9 32)) (bv 9 32)))
;; (/
;;  (bitvector->natural
;;   (bvadd
;;    (bv 8388608 32)
;;    (bvlshr
;;     (bvshl
;;      (bv #b10111110010000000000000000000000 32)
;;      (bv 9 32)
;;      )
;;     (bv 9 32))
;;    )
;;   )
;;  (expt 2 23)
;; )


;128
;1.5


; (bvshl (bv #b10111110001000000000000000000000 32) (bv #b01111111100000000000000000000000 32))
; (bitvector->natural (bv #b10111110001000000000000000000000 32))
; (bv 1 3)
; (bv #b10111110001000000000000000000000 32)
; (bit 31 (bv #b10111110001000000000000000000000 32))
;(define (fisr number)
;  (bit-field->flonum (fisr-helper (flonum->bit-field number)))
;)

; (define (fisr-helper bits)
;  (- 6910387280431496141 (arithmetic-shift (flonum->bit-field number) -1)
; (bvlshr (bvadd hi lo) (bv #x00000001 32))))

;; (define (check-fisr-helper impl bits)
;;   (define n (bit-field->flonum bits))
;;   (cond [(> n 0)
;;       (define result (bit-field->flonum (impl bits)))
;;       (define roundtrip (/ 1 (* result result)))
;;       (define err (abs (- n roundtrip)))
;;       ; (assert (<= (/ err (abs n)) 0.5))
;;       (0)
;;    ]))

(define (check-fisr-helper impl bits)
  (define n (bit-field->flonum bits))
  #t
)

;; (define (check-fisr-helper impl bits)
;;   (assert (not (bveq bits (impl bits)))))


(require rosette/lib/synthax)     ; Require the sketching library.
(define-grammar (fast-int32 x)  ; Grammar of int32 expressions over two inputs:
  [expr
   (choose x (?? int32?)        ; <expr> := x | y | <32-bit integer constant> |
           ((bop) (expr) (expr))  ;           (<bop> <expr> <expr>) |
           ((uop) (expr)))]       ;           (<uop> <expr>)
  [bop
   (choose bvadd bvsub bvand      ; <bop>  := bvadd  | bvsub | bvand |
           bvor bvxor bvshl       ;           bvor   | bvxor | bvshl |
           bvlshr bvashr)]        ;           bvlshr | bvashr
  [uop
   (choose bvneg bvnot)])         ; <uop>  := bvneg | bvnot

; sketch a fast implementation of the midpoint calculation which describes the space of
; all expressions from the fast-int32 grammar that have parse trees of depth 2 at most.
; (define (fisr-sketch n)
;   (fast-int32 n #:depth 2))

; Binds each provided identifier to a distinct symbolic constant of the given solvable type
; I.e. makes it so that l and h represent all 32 bit integers.
; (define-symbolic all_possible_int32bv int32?)

; (define sol
;     (synthesize
;      #:forall    (list all_possible_int32bv)
;      #:guarantee (check-fisr-helper fisr-sketch all_possible_int32bv)))
; (print-forms sol)
