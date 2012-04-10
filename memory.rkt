#lang racket

(require "util.rkt")

(provide memory-read
         memory-write
         memory-fill
         memory-pprint
         memory-diff)

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

(define (combine-bytes high low)
  (+ (* 256 high) low))
(define (memory-fill memory startadr vals)
  (cond 
    [(list? vals)
     (let memory-fill-list ([memory memory]
                            [startadr startadr]
                            [vals vals])
       
       (if (empty? vals)
           memory
           (memory-fill-list (memory-write memory startadr (first vals))
                             (+ 1 startadr)
                             (rest vals))))]
    [(bytes? vals)
     
     (let memory-fill-bytes ([memory memory]
                             [startadr startadr]
                             [vals (bytes->list vals)]
                             [cary -1])
       
       (cond
         [(empty? vals) memory]
         [(eq? cary -1) (memory-fill-bytes memory startadr (rest vals) (first vals))]
         [ else (memory-fill-bytes (memory-write memory
                                                 startadr 
                                                 (combine-bytes cary (first vals)))
                                   (+ 1 startadr)
                                   (rest vals)
                                   -1)]))]))


;pprint format
;0x0000: 81a1 67ae 7dc1 0006 7dc1 0014 6411 7c01
;0x0008: 8000 0402 6421 7c11 0041 0812 0481 6401
;0x0010: 8402 0191 7dc1 0001 0020 0000 0000 0000
;0x8000: 0041 0042 0043 0044 0045 0046 0047 0048

; new technique:
; 1) filter keys into sorted list of populated row indexes
; 2) build string for each row, ex:
;      0x0008: 8000 0402 6421 7c11 0041 0812 0481 6401
; 3) join rows together with newlines

(define (memory-pprint mem (cols 8))
  (define (pprint-line base-addr)
    
    (define (row-data key)
      (map (lambda (x)
             (hex-pad (memory-read mem x)))
           (build-list cols (lambda (x)
                              (+ x key)))))
    
    (string-append (row-header base-addr)
                   (string-join (row-data base-addr) " ")))
  
  (define (rem-dups in [out '()])
    (cond
      [(empty? in)    out]
      [(empty? out)   (rem-dups (cdr in) (list (car in)))]
      [(eq? (car out)
            (car in)) (rem-dups (cdr in) out)]
      [else           (rem-dups (cdr in) (list* (car in) out))]))
  
  (define (addr-row addr)
    (* (quotient addr cols) cols))
  
  (define (populated-rows addrs)
    (rem-dups (map addr-row
                   (sort addrs >))))
  
  (string-join (map pprint-line
                    (populated-rows (hash-keys mem)))
               "\n"))

; see what changed from mem1->mem2
(define (memory-diff mem1 mem2)
  (define (helper keys1 keys2 diffs)
    (if (empty? keys1)
        (append keys2 diffs)
        (let ([k1 (car keys1)]
              [k2 (car keys2)])
          (cond
            [(> k1 k2) (helper keys1 (cdr keys2) (list* k2 diffs))]; k2 doesn't exist in keys1
            [(eq? k1 k2) (if (= (memory-read mem1 k1)
                                (memory-read mem2 k2))
                             (helper (cdr keys1) (cdr keys2) diffs) ; no change
                             (helper (cdr keys1) (cdr keys2) (list* k2 diffs)))]; change
            [else (helper (cdr keys1) keys2 (list* k1 diffs))])))); k1 doesn't exist in keys2
  (helper (sort (hash-keys mem1) <=)
          (sort (hash-keys mem2) <=)
          '()))

;
;(define program (list #x7c01 #x0030 #x7de1 #x1000 #x0020 #x7803 #x1000 #xc00d
;                      #x7dc1 #x001a #xa861 #x7c01 #x2000 #x2161 #x2000 #x8463
;                      #x806d #x7dc1 #x000d #x9031 #x7c10 #x0018 #x7dc1 #x001a
;                      #x9037 #x61c1 #x7dc1 #x001a #x0000))
;(define Mem (make-immutable-hash))
;(define mem (memory-fill Mem 0 program))

;(display (memory-pprint mem))