#lang racket

(provide memory-read
         memory-write
         memory-fill)

;(define Mem (make-immutable-hash))

;forced by masking adr with #xffff
;(define (memory-valid? adr)
;  (and (<= 0 adr)
;       (> #x10000 adr)))

(define (memory-read memory adr)
    (hash-ref memory
              (bitwise-and adr #xffff)
              0))

(define (memory-write memory adr val)
    (hash-set memory
              (bitwise-and adr #xffff)
              (bitwise-and val #xffff)))

(define (memory-fill memory startadr vals)
  (if (empty? vals)
      memory
      (memory-fill (memory-write memory startadr (first vals))
                   (+ 1 startadr)
                   (rest vals))))