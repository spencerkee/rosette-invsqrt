#lang rosette

; int32? is a shorthand for the type (bitvector 32).
(define int32? (bitvector 32))
; int32 takes as input an integer literal and returns
; the corresponding 32-bit bitvector value.
(define (int32 i)
  (bv i int32?))

; (int32? 1)         ; 1 is not a 32-bit integer
; (int32? (int32 1)) ; but (int32 1) is.
; (int32 1)

; Initial implementation of bvmid function.
; Returns the midpoint of the interval [lo, hi].
(define (bvmid lo hi)  ; (lo + hi) / 2
  (bvsdiv (bvadd lo hi) (int32 2)))

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

(display "Checking bvmid against it's spec against a concrete legal input\n")
(check-mid bvmid (int32 0) (int32 0))

; Binds each provided identifier to a distinct symbolic constant of the given solvable type
; I.e. makes it so that l and h represent all 32 bit integers.
(define-symbolic l h int32?)
; verify queries the solver for a binding from symbolic constants to concrete values
; that causes the evaluation of expr to violate an assertion while astisfying all assumptions.
; If a binding exists, it's called a counterexample which is why the variable is cex.
(define cex (verify (check-mid bvmid l h)))
; We interpret the Rosette value with respect to the binding using evaluate.
(define cl (evaluate l cex))
(define ch (evaluate h cex))
(display "Return the two counterexample values in bitvector format\n")
(list cl ch)
; We can convert these values to integer? constants for debugging:
(define il (bitvector->integer cl))
(define ih (bitvector->integer ch))
(display "Return the two counterexample values in integer format\n")
(list il ih)
(display "Here is the computed midpoint:\n")
(define m (bvmid cl ch))
(bitvector->integer m)
(display "So, check-mid fails on (bvmid cl ch):\n")
; (check-mid bvmid cl ch)

; Create a bvmid-no-overflow which calculates the midpoint as
; lo + ((hi - lo) / 2) to avoid overflow.
(define (bvmid-no-overflow lo hi)
 (bvadd lo (bvsdiv (bvsub hi lo) (int32 2))))

(display "verify returns unsat on check-mid bvmid-no-overflow because there is no counterexample\n")
(verify (check-mid bvmid-no-overflow l h))


(require rosette/lib/synthax)     ; Require the sketching library.
(define-grammar (fast-int32 x y)  ; Grammar of int32 expressions over two inputs:
  [expr
   (choose x y (?? int32?)        ; <expr> := x | y | <32-bit integer constant> |
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
(define (bvmid-fast lo hi)
  (fast-int32 lo hi #:depth 2))

; Query the solver for a completion of the bvmid-fast sketch (if any) that satisfies
; our correctness specification:
(define sol
    (synthesize
     #:forall    (list l h)
     #:guarantee (check-mid bvmid-fast l h)))

; sol is now a binding from hole (i.e. non-input) constants to values such that expr satisfies
; its assertions on all legal inputs.
(display "Pretty print a syntactic representation of the completed fast sketch\n")
(print-forms sol)
