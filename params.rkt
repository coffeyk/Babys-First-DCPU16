#lang racket

(require "memory.rkt")
(require "registers.rkt")
(require "util.rkt")

(provide param-list)

(define (next-word-get mem reg)
  (memory-read mem (reg-read reg 'PC)))

; the op-...-write functions return the final (memory . registers)
(define (build-op-reg-write r-id)
  (lambda (val mem reg)
    (cons mem
          (reg-write reg r-id val))))

(define (op-mem-write val mem reg)
  (cons (memory-write mem
                      (reg-read reg 'Paadr)
                      val)
        reg))

(define (op-lit-write val mem reg)
  (cons mem reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reg is the register being updated
; returns updated register updated such that 'Pa/'Pb are set to v
; and 'Paadr/'Pbadr are set to vadr
;
; SAMPLE set-param
; 
; (lambda (reg v vadr)
;   (reg-write (reg-write reg 'Pa v)
;              'Paadr
;              vadr))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;0x00-0x07: register
(define (REG r-id)
  (lambda (mem reg set-param)
    (display (list 'REG (reg-name r-id)))
    (set-param reg (reg-read reg r-id) 0)))

;0x08-0x0f: [register]
(define (DEREF-REG r-id)
  (lambda (mem reg set-param)
    (display (list 'DEREF 'REG (reg-name r-id)))
    (let ([r-val (reg-read reg r-id)])
      ((DEREF-LIT r-val) mem reg set-param))))

(define (DEREF-LIT val)
  (lambda (mem reg set-param)
    (display (list 'DEREF val))
    (set-param reg (memory-read mem val) val)))

;0x10-0x17: [next word + register]
; [next word + register]
(define (DEREF-NW+ r-id)
  (lambda (mem reg set-param)
    (display (list 'DEREF 'NW+ (reg-name r-id)))
    (let ([calc-val (+ (next-word-get mem reg)
                       (reg-read reg r-id))]
          [new-reg (reg-inc reg 'PC)])
      (printf "calc-addr:~x\n" calc-val)
      (reg-inc ((DEREF-LIT calc-val) mem new-reg set-param)
               'CLK))))

;0x18: POP / [SP++]
(define (POP)
  (lambda (mem reg set-param)
    (display (list 'POP))
    (reg-inc ((DEREF-REG 'SP) mem reg set-param)
             'SP)))

;0x19: PEEK / [SP]
(define (PEEK)
  (DEREF-REG 'SP))

;0x1a: [--SP]
(define (PUSH)
  (lambda (mem reg set-param)
    (display (list 'PUSH))
    ((DEREF-REG 'SP) mem 
                     (reg-dec reg 'SP)
                     set-param)))

;0x1b: SP
(define (SP)
  (REG 'SP))

;0x1c: PC
(define (PC)
  (REG 'PC))

;0x1d: O
(define (O)
  (REG 'O))

;0x1e: [next word] === [[PC++]]
(define (DEREF-NW)
  (lambda (mem reg set-param)
    (display (list 'DEREF 'NW))
    (let ([nw (next-word-get mem reg)]
          [new-reg (reg-inc reg 'PC)])
      (reg-inc ((DEREF-LIT nw) mem new-reg set-param)
               'CLK))))

;0x1f: next word [PC++]
(define (NW)
  (lambda (mem reg set-param)
    (display (list 'NW))
    (reg-inc (reg-inc ((DEREF-REG 'PC) mem reg set-param)
             'PC)
             'CLK)))

;0x20-0x3f: literal 0x00-0x1f
(define (LIT val)
  (lambda (mem reg set-param)
    (display (list val))
    (set-param reg val 0)))

(define param-list 
  (build-list #x40 
              (lambda (var-id)
                (cond
                  [(in-between  #x0 var-id #x7)  (list (REG (reg-name var-id))
                                                       0
                                                       (build-op-reg-write var-id))]
                  [(in-between  #x8 var-id  #xf) (list (DEREF-REG (reg-name (- var-id #x8)))
                                                       0
                                                       op-mem-write)]
                  [(in-between #x10 var-id #x17) (list (DEREF-NW+ (reg-name (- var-id #x10)))
                                                       1
                                                       op-mem-write)]
                  [(= #x18 var-id) (list (POP)
                                         0
                                         op-mem-write)]
                  [(= #x19 var-id) (list (PEEK)
                                         0
                                         op-mem-write)]
                  [(= #x1a var-id) (list (PUSH)
                                         0
                                         op-mem-write)]
                  [(in-between #x1b var-id #x1d) (list (REG (- var-id 19))
                                                       0
                                                       (build-op-reg-write (reg-name (- var-id 19))))]
                  [(= #x1e var-id) (list (DEREF-NW)
                                         1
                                         op-mem-write)]
                  [(= #x1f var-id) (list (NW)
                                         1
                                         op-lit-write)]
                  [(in-between #x20 var-id #x3f) (list (LIT (- var-id #x20))
                                                       0
                                                       op-lit-write)]
                  [else (error "invalid Value" var-id)]))))