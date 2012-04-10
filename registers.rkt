#lang racket

(require "util.rkt")

(provide build-reg
         reg-name
         reg-id
         reg-read
         reg-write
         reg-inc
         reg-dec
         reg-pprint)

;; a-list of registers and initial values
(define (build-reg)
  '((A . 0)
    (B . 0)
    (C . 0) 
    (X . 0)
    (Y . 0)
    (Z . 0)
    (I . 0)
    (J . 0)
    (SP . 0)
    (PC . 0)
    (O . 0)
    (Pa . 0)
    (Pb . 0)
    (Paadr . 0)
    (Pbadr . 0) 
    (CLK . 0)))

(define *REF-REG* (build-reg)) ; used to determine structure

(define (reg-name n)
  (if (symbol? n)
      n
      (car (list-ref *REF-REG* n))))

(define (reg-id s)
  (- (length *REF-REG*)
     (length (memf (lambda (arg)
                     (eq? s (car arg)))
                   *REF-REG*))))

(define (reg-write reg r val)
  (let ([r-name (reg-name r)])
    (define (reg-write-helper entry)
      (if (eq? r-name
               (car entry))
          (cons r-name val)
          entry))
    (map reg-write-helper reg)))

(define (reg-read reg r)
  (bitwise-and (cdr (assoc (reg-name r) reg))
               #xffff))
  

(define (reg-inc reg r-id)
  (reg-write reg r-id (+ 1 (reg-read reg r-id))))

(define (reg-dec reg r-id)
  (reg-write reg r-id (- (reg-read reg r-id) 1)))

(define (reg-pprint reg)
  (let pprint ([l reg]
               [out-str ""])
    (if (empty? l)
        out-str
        (pprint (cdr l) (string-append out-str
                                       (string-pad (format "~a" (caar l)) 5)
                                       ":"
                                       (hex-pad (bitwise-and (cdar l)
                                                             #xffff))
                                       "\n")))))
                                        
                                        