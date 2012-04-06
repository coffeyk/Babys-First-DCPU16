#lang racket

;(require "registers.rkt") ;old structure based registers
(require "registers2.rkt") ; new list based registers
(require "memory.rkt")

;(define Reg (registers 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define Reg (build-reg))

(define Mem (make-immutable-hash))
;(define Reg (make-immutable-hash))

;(define (reg-valid? r)
;  (member r ('A 'B 'C 'D 'X 'Y 'Z 'I 'J 'SP 'PC 'O)))

; opcode format bbbbbbaaaaaaoooo
(define (fetch-opcode hex)
  (case (bitwise-and #xf hex)
    [(#x0) 'nonbasic]
    [(#x1) 'SET]
    [(#x2) 'ADD]
    [(#x3) 'SUB]
    [(#x4) 'MUL]
    [(#x5) 'DIV]
    [(#x6) 'MOD]
    [(#x7) 'SHL]
    [(#x8) 'SHR]
    [(#x9) 'AND]
    [(#xa) 'BOR]
    [(#xb) 'XOR]
    [(#xc) 'IFE]
    [(#xd) 'IFN]
    [(#xe) 'IFG]
    [(#xf) 'IFB]))
; non-basic opcode format aaaaaaoooooo0000
(define (fetch-opcode-nonbasic hex)
  (case (bitwise-and #b111111
                     (arithmetic-shift hex -4))
    [(#x01) 'JSR]
    [else (error "Reserved opcode" hex)]))


(define (build-op-write hex)
  (let ([i (extract-var-a hex)])
    (cond
      [(in-between  #x0 i  #x7) (build-op-reg-write (reg-name i))]
      [(in-between  #x8 i  #xf) op-mem-write]
      [(in-between #x10 i #x17) op-mem-write]
      [(= #x18 i)               op-mem-write]
      [(= #x19 i)               op-mem-write]
      [(= #x1a i)               op-mem-write]
      [(in-between #x1b i #x1d) (build-op-reg-write (reg-name (- i 19)))]
      [(= #x1e i)               op-mem-write]
      [(= #x1f i)               op-lit-write]
      [(in-between #x20 i #x3f) op-lit-write]
      [else (error "invalid Value")]
      )))


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
  (cons mem
        reg))


(define (extract-var-a hex)
  (bitwise-bit-field hex 4 10))

(define (extract-var-b hex)
  (bitwise-bit-field hex 10 16))

(define (var-size v)
  (cond
    [(in-between #x10 v #x17) 1]
    [(= #x1e v) 1]
    [(= #x1f v) 1]
    [else 0]))

(define (calc-clock-nonbasic hex)
  (let ([s (fetch-opcode-nonbasic hex)])
    (+ (isr-size hex)
       (case s 
         ['JSR 1]))))

(define (calc-clock hex)
  (let ([s (fetch-opcode hex)])
    (if (eq? 'nonbasic s)
        (calc-clock-nonbasic hex)
        ; clock cycles are (isr size + isr cost)
        (+ (isr-size hex)
           (case s 
             ['SET 0]
             ['ADD 1]
             ['SUB 1]
             ['MUL 1]
             ['DIV 2]
             ['MOD 2]
             ['SHL 1]
             ['SHR 1]
             ['AND 0]
             ['BOR 0]
             ['XOR 0]
             ['IFE 1]
             ['IFN 1]
             ['IFG 1]
             ['IFB 1])
           ))))

(define (clock-cycle reg clks)
  (let ([clk (reg-read reg 'CLK)])
    (reg-write reg
               'CLK
               (+ clk clks))))


(define (isr-size-nonbasic hex)
  (let ([vb (var-size (extract-var-b hex))]
        [s (fetch-opcode-nonbasic hex)])
    (case s 
      [else (+ 1 vb)])))

(define (isr-size hex)
  (let ([va (var-size (extract-var-a hex))]
        [vb (var-size (extract-var-b hex))]
        [s (fetch-opcode hex)])
    (case s
      ['nonbasic (isr-size-nonbasic hex)]
      [else      (+ 1 va vb)])))

(define (skip-isr hex reg)
  (let* ([va (var-size (extract-var-a hex))]
         [vb (var-size (extract-var-b hex))]
         [i-size (isr-size hex)])
    (reg-write reg 
               'PC
               (+ (reg-read reg 'PC) i-size))))

; 0x1: SET a, b - sets a to b
(define (SET cycles mem reg w-func)
  (let ([val (reg-read reg 'Pb)])
    (w-func val
            mem
            (clock-cycle reg
                         cycles))))

; 0x2: ADD a, b - sets a to a+b, sets O to 0x0001 if there's an overflow, 0x0 otherwise
(define (ADD cycles mem reg w-func)
  (let* ([val (+ (reg-read reg 'Pa)
                 (reg-read reg 'Pb))]
         [o-val (if (> val #xffff)
                    1
                    0)])
    (w-func val
            mem
            (clock-cycle (reg-write reg 'O o-val)
                         cycles))))

; 0x3: SUB a, b - sets a to a-b, sets O to 0xffff if there's an underflow, 0x0 otherwise
(define (SUB cycles mem reg w-func)
  (let* ([pa (reg-read reg 'Pa)]
         [pb (reg-read reg 'Pb)]
         [val (- pa pb)]
         [o-val (if (< val 0)
                    #xffff
                    0)])
    (w-func val
            mem
            (clock-cycle (reg-write reg 'O o-val)
                         cycles))))

; 0x4: MUL a, b - sets a to a*b, sets O to ((a*b)>>16)&0xffff
(define (MUL cycles mem reg w-func)
  (let* ([val (* (reg-read reg 'Pa)
                 (reg-read reg 'Pb))]
         [o-val (arithmetic-shift val -16)])
    (w-func val 
            mem 
            (clock-cycle (reg-write reg 'O o-val)
                         cycles))))

; 0x5: DIV a, b - sets a to a/b, sets O to ((a<<16)/b)&0xffff. if b==0, sets a and O to 0 instead.
(define (DIV cycles mem reg w-func)
  (let ([pa (reg-read reg 'Pa)]
        [pb (reg-read reg 'Pb)])
    (if (= 0 pb )
        (w-func 0 
                mem 
                (clock-cycle (reg-write reg 'O 0)
                             cycles))
        (let ([val (/ pa pb)]
              [o-val (/ (arithmetic-shift pa -16)
                        pb)])
          (w-func val 
                  mem 
                  (clock-cycle (reg-write reg 'O o-val)
                               cycles))))))

; 0x6: MOD a, b - sets a to a%b. if b==0, sets a to 0 instead.
(define (MOD cycles mem reg w-func)
  (let ([pa (reg-read reg 'Pa)]
        [pb (reg-read reg 'Pb)])
    (if (= 0 pb)
        (w-func 0
                mem
                (clock-cycle reg
                             cycles))
        (let ([val (remainder (reg-read reg 'Pa)
                              (reg-read reg 'Pb))])
          (w-func val
                  mem
                  (clock-cycle reg
                               cycles))))))

; 0x7: SHL a, b - sets a to a<<b, sets O to ((a<<b)>>16)&0xffff
(define (SHL cycles mem reg w-func)
  (let* ([val (arithmetic-shift (reg-read reg 'Pa)
                                (reg-read reg 'Pb))]
         [o-val (arithmetic-shift val -16)])
    (w-func val 
            mem 
            (clock-cycle (reg-write reg 'O o-val)
                         cycles))))

; 0x8: SHR a, b - sets a to a>>b, sets O to ((a<<16)>>b)&0xffff
(define (SHR cycles mem reg w-func)
  (let* ([pa (reg-read reg 'Pa)]
         [pb (reg-read reg 'Pb)]
         [val (arithmetic-shift pa (- pb))]
         [o-val (arithmetic-shift (arithmetic-shift pa 16)
                                  (- pb))])
    (w-func val 
            mem 
            (clock-cycle (reg-write reg 'O o-val)
                         cycles))))

; 0x9: AND a, b - sets a to a&b
(define (AND cycles mem reg w-func)
  (let* ([val (bitwise-and (reg-read reg 'Pa)
                           (reg-read reg 'Pb))])
    (w-func val
            mem
            (clock-cycle reg
                         cycles))))

; 0xa: BOR a, b - sets a to a|b
(define (BOR cycles mem reg w-func)
  (let* ([val (bitwise-ior (reg-read reg 'Pa)
                           (reg-read reg 'Pb))])
    (w-func val
            mem
            (clock-cycle reg
                         cycles))))
; 0xb: XOR a, b - sets a to a^b
(define (XOR cycles mem reg w-func)
  (let* ([val (bitwise-xor (reg-read reg 'Pa)
                           (reg-read reg 'Pb))])
    (w-func val
            mem
            (clock-cycle reg
                         cycles))))

; IFE, IFN, IFG, and IFB don't make use of w-func,
; instead forcing a 'literal' write, which doesn't write
; 0xc: IFE a, b - performs next instruction only if a==b
(define (IFE cycles mem reg w-func)
  (let ([pa (reg-read reg 'Pa)]
        [pb (reg-read reg 'Pb)]
        [hex (memory-read mem
                          (reg-read reg 'PC))])
    (if (= pa pb)
        (op-lit-write 0 
                      mem 
                      (clock-cycle reg
                                   cycles))
        (op-lit-write 0 
                      mem 
                      (clock-cycle (skip-isr hex reg)
                                   (+ 1 cycles)))))) ; skip next instruction & values
; 0xd: IFN a, b - performs next instruction only if a!=b
(define (IFN cycles mem reg w-func)
  (let ([pa (reg-read reg 'Pa)]
        [pb (reg-read reg 'Pb)]
        [hex (memory-read mem
                          (reg-read reg 'PC))])
    (if (= pa pb)
        (op-lit-write 0 
                      mem 
                      (clock-cycle (skip-isr hex reg)
                                   (+ 1 cycles))) ; skip next instruction & values
        (op-lit-write 0 
                      mem 
                      (clock-cycle reg
                                   cycles)))))
; 0xe: IFG a, b - performs next instruction only if a>b
(define (IFG cycles mem reg w-func)
  (let ([pa (reg-read reg 'Pa)]
        [pb (reg-read reg 'Pb)]
        [hex (memory-read mem
                          (reg-read reg 'PC))])
    (if (> pa pb)
        (op-lit-write 0 
                      mem 
                      (clock-cycle reg
                                   cycles))
        (op-lit-write 0 
                      mem 
                      (clock-cycle (skip-isr hex reg)
                                   (+ 1 cycles)))))) ; skip next instruction & values
; 0xf: IFB a, b - performs next instruction only if (a&b)!=0
(define (IFB cycles mem reg w-func)
  (let ([val (bitwise-and (reg-read reg 'Pa)
                          (reg-read reg 'Pb))]
        [hex (memory-read mem
                          (reg-read reg 'PC))])
    (if (= val 0)
        (op-lit-write 0 
                      mem 
                      (clock-cycle (skip-isr hex reg)
                                   (+ 1 cycles))) ; skip next instruction & values
        (op-lit-write 0 
                      mem 
                      (clock-cycle reg
                                   cycles)))))


; 0x01 JSR a - pushes the address of the next instruction to the stack, then sets PC to a
(define (JSR cycles mem reg)
  (let* ([new-reg (reg-dec reg 'SP)]
         [pc (reg-read new-reg 'PC)])
    ((build-op-reg-write 'PC) (reg-read reg 'Pb)
                              (memory-write mem
                                            (reg-read new-reg 'SP)
                                            pc)
                              (clock-cycle new-reg
                                           cycles))))


(define (call-nonbasic-hex hex mem reg)
  (let ([s (fetch-opcode-nonbasic hex)]
        [cycles (calc-clock hex)])
    (display s)
    (case s
      ['JSR (JSR cycles mem reg)])))

(define (call-hex hex mem reg)
  (let ([w-func (build-op-write hex)]
        [s (fetch-opcode hex)]
        [cycles (calc-clock hex)])
    (display s)
    (display "\n")
    (case s 
      ['nonbasic  (call-nonbasic-hex hex mem reg)]
      ['SET  (SET cycles mem reg w-func)]
      ['ADD  (ADD cycles mem reg w-func)]
      ['SUB  (SUB cycles mem reg w-func)]
      ['MUL  (MUL cycles mem reg w-func)]
      ['DIV  (DIV cycles mem reg w-func)]
      ['MOD  (MOD cycles mem reg w-func)]
      ['SHL  (SHL cycles mem reg w-func)]
      ['SHR  (SHR cycles mem reg w-func)]
      ['AND  (AND cycles mem reg w-func)]
      ['BOR  (BOR cycles mem reg w-func)]
      ['XOR  (XOR cycles mem reg w-func)]
      ['IFE  (IFE cycles mem reg w-func)]
      ['IFN  (IFN cycles mem reg w-func)]
      ['IFG  (IFG cycles mem reg w-func)]
      ['IFB  (IFB cycles mem reg w-func)])))

(define (in-between a v b)
  (and (<= a v)
       (<= v b)))

; sample set-param
; reg is the register structure being updated
; returns a structure updated such that register 'Pa/'Pb are set to v
; lambda(v) (reg-write r (reg-id 'Pa) v) ; write parameter a
; lambda(v) (reg-write r (reg-id 'Pb) v) ; write parameter b
(define (update-var var-id mem reg set-param)
  ;0x00-0x07: register
  (define (REG r-id)
    (display (list 'REG (reg-name r-id)))
    (set-param (reg-read reg r-id)
               0
               reg))
  ;0x08-0x0f: [register]
  (define (DEREF-REG r-id)
    (display (list 'DEREF 'REG (reg-name r-id)))
    (let ([addr (reg-read reg r-id)])
      (set-param (memory-read mem addr)
                 addr
                 reg)))
  ;0x10-0x17: [next word + register]
  ; [next word + register]
  (define (DEREF-NW+ r-id)
    (display (list 'DEREF 'NW+ (reg-name r-id)))
    (let* ([nw (memory-read mem (reg-read reg 'PC))]
           [r-val (reg-read reg r-id)]
           [calc-addr (+ nw r-val)])
      (display "calc-addr")
      (display calc-addr)
      (display "\n")
      (reg-inc (set-param (memory-read mem calc-addr)
                          calc-addr
                          reg)
               'PC)))
  ;after reg-inc, sp-val is no longer valid in this context\
  ;0x18: POP / [SP++]
  (define (POP)
    (display (list 'POP))
    (let ([sp (reg-read reg 'SP)])
      (reg-inc (set-param (memory-read mem sp)
                          sp
                          reg)
               'SP)))
  ;0x19: PEEK / [SP]
  (define (PEEK)
    (display (list 'PEEK))
    (DEREF-REG 'SP))
  ; fake the fact that
  ; sp-val = [--SP]
  (define (PUSH)
    (display (list 'PUSH))
    (let ([sp-val (- (reg-read reg 'SP) 1)])
      (reg-dec (set-param (memory-read mem sp-val)
                          sp-val
                          reg)
               'SP)))
  (define (SP)
    (REG 'SP))
  (define (PC)
    (REG 'PC))
  (define (O)
    (REG 'O))
  (define (DEREF-NW)
    (display (list 'DEREF 'NW))
    (let ([adr (memory-read mem (reg-read reg 'PC))])
      (reg-inc (set-param (memory-read mem adr)
                          adr
                          reg)
               'PC)))
  (define (NW)
    (display (list 'NW))
    (let ([pc (reg-read reg 'PC)])
      (reg-inc (set-param (memory-read mem pc)
                          0
                          reg)
               'PC)))
  (define (LIT val)
    (display (list val))
    (set-param val
               0
               reg))
  
  (cond
    [(in-between  #x0 var-id #x7)  (REG (reg-name var-id))]
    [(in-between  #x8 var-id  #xf) (DEREF-REG (reg-name (- var-id #x8)))]
    [(in-between #x10 var-id #x17) (DEREF-NW+ (reg-name (- var-id #x10)))]
    [(= #x18 var-id) (POP)]
    [(= #x19 var-id) (PEEK)]
    [(= #x1a var-id) (PUSH)]
    [(= #x1b var-id) (SP)]
    [(= #x1c var-id) (PC)]
    [(= #x1d var-id) (O)]
    ;[(in-between #x1b var-id #x1d) (REG (- var-id 19))]
    [(= #x1e var-id) (DEREF-NW)]
    [(= #x1f var-id) (NW)]
    [(in-between #x20 var-id #x3f) (LIT (- var-id #x20))]
    [else (error "invalid Value")])
  )

(define (parse-vars hex ev-count mem reg)
  ;#b0000001111110000
  (define (reg-update-a)
    (update-var (extract-var-a hex) 
                mem 
                reg 
                (lambda (v vadr reg)
                  (printf "Pa:~x Paadr:~x\n" v vadr)
                  (reg-write (reg-write reg 'Pa v)
                             'Paadr
                             vadr))))
  ;#b1111110000000000
  (define (reg-update-b)
    (update-var (extract-var-b hex)
                mem
                reg
                (lambda (v vadr reg)
                  (printf "Pb:~x Pbadr:~x\n" v vadr)
                  (reg-write (reg-write reg 'Pb v)
                             'Pbadr
                             vadr)))) 
  (case ev-count
    [(2)
     ;update registers with new value for Pa
     (parse-vars hex
                 (- ev-count 1)
                 mem 
                 (reg-update-a))]
    [(1)
     ;execute the hex with new value for Pb
     (call-hex hex
               mem
               (reg-update-b))]))

(define (parse-pc mem reg)
  (let* ([pc-val (reg-read reg 'PC)]
         [hex (memory-read mem pc-val)]
         [ev-count (if (eq? (fetch-opcode hex) 'nonbasic)
                       1
                       2)])
    ;(if (>= pc-val 8)
    ;(cons mem reg)
    (parse-vars hex
                ev-count
                mem
                (reg-inc reg 'PC))));)

;(fetch-opcode (memory-read (memory-fill Mem 0 program) (reg-read Reg 'PC)))
;(reg-write Reg 'Pa 123)
(define (run mem-reg oldpc)
  (let* ([mem (car mem-reg)]
         [reg (cdr mem-reg)]
         [pc (reg-read reg 'PC)])
    (if (= oldpc pc)
        mem-reg
        (begin (display "\n")
               (display reg)
               (display "\n")
               (display mem)
               (display "\n")
               (run (parse-pc mem reg) pc)))))


;Sample ASM code to be loaded
(define program (list #x7c01 #x0030 #x7de1 #x1000 #x0020 #x7803 #x1000 #xc00d
                      #x7dc1 #x001a #xa861 #x7c01 #x2000 #x2161 #x2000 #x8463
                      #x806d #x7dc1 #x000d #x9031 #x7c10 #x0018 #x7dc1 #x001a
                      #x9037 #x61c1 #x7dc1 #x001a #x0000))

; fill memory starting at #x0 from program
(memory-fill Mem 0 program)

; execute program until PC is static
(run (cons (memory-fill Mem 0 program) Reg) -1)