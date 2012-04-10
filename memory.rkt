#lang racket

(provide memory-read
         memory-write
         memory-fill
         memory-pprint)

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

(define (string-pad str width [pad #\space])
  (define field-width (min width (string-length str)))
  (define lmargin (- width field-width))
  (string-append (build-string lmargin (lambda (x) pad))
                 str))



;pprint format
;0x0000: 81a1 67ae 7dc1 0006 7dc1 0014 6411 7c01
;0x0008: 8000 0402 6421 7c11 0041 0812 0481 6401
;0x0010: 8402 0191 7dc1 0001 0020 0000 0000 0000
;0x8000: 0041 0042 0043 0044 0045 0046 0047 0048

;print-addr-colum
;zero-filler up to key-idx
;print-value-string+
;zero-filler?
;new-line

;no more keys - fill out row with zeros; return
; gap? - fill in gap with zeros up to end of row; call again with last zero idx as last key
; new row - print \n
;         - print row info
;         - fill in left zeros
;         - prit value
;         - call again with key as last key


(define (memory-pprint mem)
  (define rows 8); rename cols to be accurate
  
  (define (hex-pad x)
    (string-pad (format "~x" x) 4 #\0))
  
  (define (zero-filler x)
    (string-join (list* "" (build-list x (lambda (x) (hex-pad 0)))) " "))
  
  (define (row-header key)
    (string-append (hex-pad (- key (remainder key rows))) ":"))
  
  (define (pprint keys last-key out-str)
    (let ([last-key-idx (remainder last-key rows)]
          [last-key-row (quotient last-key rows)])
      (if (empty? keys)
          (string-append out-str
                         ;" %"
                         (zero-filler (- rows last-key-idx 1))
                         "\n"); pad out to end of line
          (let* ([key (car keys)]
                 [key-idx (remainder key rows)]
                 [key-row (quotient key rows)]
                 [value-string (hex-pad (memory-read mem key))])
            (cond 
              [(not (eq? key-row last-key-row))
               (pprint (cdr keys) key (string-append out-str
                                                     (zero-filler (- rows last-key-idx 1))
                                                     "\n"
                                                     (row-header key)
                                                     (zero-filler (max 0 (- key-idx 0)))
                                                     " "
                                                     ; (hex-pad (memory-read mem key))))]
                                                     value-string))]
              [(< 1 (- key-idx last-key-idx))
               (pprint keys (- key 1) (string-append out-str
                                                     " "
                                                     (zero-filler (- key-idx last-key-idx 1))
                                                     ;" "
                                                     ;(hex-pad (memory-read mem key))))])))))
                                                     ;(hex-pad key)))
                                                     ))]
              [else
               (pprint (cdr keys) key (string-append out-str
                                                     " "
                                                     ;(hex-pad (memory-read mem key))))])))))
                                                     value-string))])))))
  
  (pprint (sort (hash-keys mem) <) 1  (row-header 0)))
;
;(define program (list #x7c01 #x0030 #x7de1 #x1000 #x0020 #x7803 #x1000 #xc00d
;                      #x7dc1 #x001a #xa861 #x7c01 #x2000 #x2161 #x2000 #x8463
;                      #x806d #x7dc1 #x000d #x9031 #x7c10 #x0018 #x7dc1 #x001a
;                      #x9037 #x61c1 #x7dc1 #x001a #x0000))
;(define Mem (make-immutable-hash))
;(define mem (memory-fill Mem 0 program))

;(display (memory-pprint mem))