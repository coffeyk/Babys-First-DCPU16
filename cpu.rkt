#lang racket

(require "registers.rkt") ; new list based registers
(require "memory.rkt")

(provide step-cpu
         calc-instruction-size)

(struct instruction
  (opcode
   op-func
   var-list
   basic?))

; returns the appropriate function to write to the Value
(define (build-op-write var-id)
  (cond
    [(in-between  #x0 var-id  #x7) (build-op-reg-write (reg-name var-id))]
    [(in-between  #x8 var-id  #xf) op-mem-write]
    [(in-between #x10 var-id #x17) op-mem-write]
    [(= #x18 var-id)               op-mem-write]
    [(= #x19 var-id)               op-mem-write]
    [(= #x1a var-id)               op-mem-write]
    [(in-between #x1b var-id #x1d) (build-op-reg-write (reg-name (- var-id 19)))]
    [(= #x1e var-id)               op-mem-write]
    [(= #x1f var-id)               op-lit-write]
    [(in-between #x20 var-id #x3f) op-lit-write]
    [else (error "invalid Value")]))

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

; extract the value A id from the hex instruction
(define (extract-var-a hex)
  (bitwise-bit-field hex 4 10))

; extract the value B id from the hex instruction
(define (extract-var-b hex)
  (bitwise-bit-field hex 10 16))

; construct an instruction based on the hex code
(define (hex->instruction hex)
  (if (eq? (car (fetch-opcode hex))
           'nonbasic)
      (let ([op (fetch-opcode-nonbasic hex)])
        (instruction (car op); symbol
                     (cdr op); op-func to be called
                     (list (extract-var-b hex))
                     #f)) ; nonbasic instruction
      (let ([op (fetch-opcode hex)])
        (instruction (car op); symbol
                     (cdr op); op-func to be called
                     (list (extract-var-b hex) ; reverse order to accomodate nonbasic
                           (extract-var-a hex))
                     #t)))) ; basic instruction

; returns the instruction at 'PC
; does not increment 'PC
; read-next-instruction -> instruction
(define (read-next-instruction mem reg)
  (let* ([pc-val (reg-read reg 'PC)]
         [hex (memory-read mem pc-val)])
    (hex->instruction hex)))

; adds clks to the 'CLK register
; clock-cycle -> register
(define (clock-cycle reg clks)
  (let ([clk (reg-read reg 'CLK)])
    (reg-write reg
               'CLK
               (+ clk clks))))

; calculates the size of an instruction in words, based on the varlist ids
(define (isr-size varlist)
  (foldl + 1 (map (lambda (var-id)
                    (cond
                      [(in-between #x10 var-id #x17) 1]
                      [(= #x1e var-id) 1]
                      [(= #x1f var-id) 1]
                      [else 0]))
                  varlist)))

; increment the 'PC to the start of the next instruction
(define (skip-isr oplist reg)
  (let ([varlist (instruction-var-list oplist)])
    (reg-write reg 
               'PC
               (+ (reg-read reg 'PC) (isr-size varlist)))))

; 0x1: SET a, b - sets a to b
(define (SET cycles w-func)
  (define op-cycles (+ 0 cycles))
  (lambda (mem reg)
    (let ([val (reg-read reg 'Pb)])
      (w-func val
              mem
              (clock-cycle reg
                           cycles)))))

; 0x2: ADD a, b - sets a to a+b, sets O to 0x0001 if there's an overflow, 0x0 otherwise
(define (ADD cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let* ([val (+ (reg-read reg 'Pa)
                   (reg-read reg 'Pb))]
           [o-val (if (> val #xffff)
                      1
                      0)])
      (w-func val
              mem
              (clock-cycle (reg-write reg 'O o-val)
                           op-cycles)))))

; 0x3: SUB a, b - sets a to a-b, sets O to 0xffff if there's an underflow, 0x0 otherwise
(define (SUB cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let* ([pa (reg-read reg 'Pa)]
           [pb (reg-read reg 'Pb)]
           [val (- pa pb)]
           [o-val (if (< val 0)
                      #xffff
                      0)])
      (w-func val
              mem
              (clock-cycle (reg-write reg 'O o-val)
                           op-cycles)))))

; 0x4: MUL a, b - sets a to a*b, sets O to ((a*b)>>16)&0xffff
(define (MUL cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let* ([val (* (reg-read reg 'Pa)
                   (reg-read reg 'Pb))]
           [o-val (arithmetic-shift val -16)])
      (w-func val 
              mem 
              (clock-cycle (reg-write reg 'O o-val)
                           op-cycles)))))

; 0x5: DIV a, b - sets a to a/b, sets O to ((a<<16)/b)&0xffff. if b==0, sets a and O to 0 instead.
(define (DIV cycles w-func)
  (define op-cycles (+ 2 cycles))
  (lambda (mem reg)
    (let ([pa (reg-read reg 'Pa)]
          [pb (reg-read reg 'Pb)])
      (if (= 0 pb )
          (w-func 0 
                  mem 
                  (clock-cycle (reg-write reg 'O 0)
                               op-cycles))
          (let ([val (/ pa pb)]
                [o-val (/ (arithmetic-shift pa -16)
                          pb)])
            (w-func val 
                    mem 
                    (clock-cycle (reg-write reg 'O o-val)
                                 op-cycles)))))))

; 0x6: MOD a, b - sets a to a%b. if b==0, sets a to 0 instead.
(define (MOD cycles w-func)
  (define op-cycles (+ 2 cycles))
  (lambda (mem reg)
    (let ([pa (reg-read reg 'Pa)]
          [pb (reg-read reg 'Pb)])
      (if (= 0 pb)
          (w-func 0
                  mem
                  (clock-cycle reg
                               op-cycles))
          (let ([val (remainder (reg-read reg 'Pa)
                                (reg-read reg 'Pb))])
            (w-func val
                    mem
                    (clock-cycle reg
                                 op-cycles)))))))

; 0x7: SHL a, b - sets a to a<<b, sets O to ((a<<b)>>16)&0xffff
(define (SHL cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let* ([val (arithmetic-shift (reg-read reg 'Pa)
                                  (reg-read reg 'Pb))]
           [o-val (arithmetic-shift val -16)])
      (w-func val 
              mem 
              (clock-cycle (reg-write reg 'O o-val)
                           op-cycles)))))

; 0x8: SHR a, b - sets a to a>>b, sets O to ((a<<16)>>b)&0xffff
(define (SHR cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let* ([pa (reg-read reg 'Pa)]
           [pb (reg-read reg 'Pb)]
           [val (arithmetic-shift pa (- pb))]
           [o-val (arithmetic-shift (arithmetic-shift pa 16)
                                    (- pb))])
      (w-func val 
              mem 
              (clock-cycle (reg-write reg 'O o-val)
                           op-cycles)))))

; 0x9: AND a, b - sets a to a&b
(define (AND cycles w-func)
  (define op-cycles (+ 0 cycles))
  (lambda (mem reg)
    (let* ([val (bitwise-and (reg-read reg 'Pa)
                             (reg-read reg 'Pb))])
      (w-func val
              mem
              (clock-cycle reg
                           op-cycles)))))

; 0xa: BOR a, b - sets a to a|b
(define (BOR cycles w-func)
  (define op-cycles (+ 0 cycles))
  (lambda (mem reg)
    (let* ([val (bitwise-ior (reg-read reg 'Pa)
                             (reg-read reg 'Pb))])
      (w-func val
              mem
              (clock-cycle reg
                           op-cycles)))))
; 0xb: XOR a, b - sets a to a^b
(define (XOR cycles w-func)
  (define op-cycles (+ 0 cycles))
  (lambda (mem reg)
    (let* ([val (bitwise-xor (reg-read reg 'Pa)
                             (reg-read reg 'Pb))])
      (w-func val
              mem
              (clock-cycle reg
                           op-cycles)))))

; IFE, IFN, IFG, and IFB don't make use of w-func,
; instead forcing a 'literal' write, which doesn't write
; 0xc: IFE a, b - performs next instruction only if a==b
(define (IFE cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let ([pa (reg-read reg 'Pa)]
          [pb (reg-read reg 'Pb)]
          [n-oplist (read-next-instruction mem reg)])
      (if (= pa pb)
          (op-lit-write 0 
                        mem 
                        (clock-cycle reg
                                     op-cycles))
          (op-lit-write 0 
                        mem 
                        (clock-cycle (skip-isr n-oplist reg)
                                     (+ 1 op-cycles))))))) ; skip next instruction & values
; 0xd: IFN a, b - performs next instruction only if a!=b
(define (IFN cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let ([pa (reg-read reg 'Pa)]
          [pb (reg-read reg 'Pb)]
          [n-oplist (read-next-instruction mem reg)])
      (if (= pa pb)
          (op-lit-write 0 
                        mem 
                        (clock-cycle (skip-isr n-oplist reg)
                                     (+ 1 op-cycles))) ; skip next instruction & values
          (op-lit-write 0 
                        mem 
                        (clock-cycle reg
                                     op-cycles))))))
; 0xe: IFG a, b - performs next instruction only if a>b
(define (IFG cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let ([pa (reg-read reg 'Pa)]
          [pb (reg-read reg 'Pb)]
          [n-oplist (read-next-instruction mem reg)])
      (if (> pa pb)
          (op-lit-write 0 
                        mem 
                        (clock-cycle reg
                                     op-cycles))
          (op-lit-write 0 
                        mem 
                        (clock-cycle (skip-isr n-oplist reg)
                                     (+ 1 op-cycles))))))) ; skip next instruction & values
; 0xf: IFB a, b - performs next instruction only if (a&b)!=0
(define (IFB cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let ([val (bitwise-and (reg-read reg 'Pa)
                            (reg-read reg 'Pb))]
          [n-oplist (read-next-instruction mem reg)])
      (if (= val 0)
          (op-lit-write 0 
                        mem 
                        (clock-cycle (skip-isr n-oplist reg)
                                     (+ 1 op-cycles))) ; skip next instruction & values
          (op-lit-write 0 
                        mem 
                        (clock-cycle reg
                                     op-cycles))))))


; 0x01 JSR a - pushes the address of the next instruction to the stack, then sets PC to a
; actually used Pb for simplicity's sake
(define (JSR cycles w-func)
  (define op-cycles (+ 1 cycles))
  (lambda (mem reg)
    (let* ([new-reg (reg-dec reg 'SP)]
           [pc (reg-read new-reg 'PC)])
      ((build-op-reg-write 'PC) (reg-read reg 'Pb)
                                (memory-write mem
                                              (reg-read new-reg 'SP)
                                              pc)
                                (clock-cycle new-reg
                                             op-cycles)))))

; opcode format bbbbbbaaaaaaoooo
; returns the symbol and op-func for a basic hex instruction
(define (fetch-opcode hex)
  (define opcode-list
    (list 
     (cons 'nonbasic error)
     (cons 'SET SET)
     (cons 'ADD ADD) 
     (cons 'SUB SUB)
     (cons 'MUL MUL)
     (cons 'DIV DIV)
     (cons 'MOD MOD)
     (cons 'SHL SHL)
     (cons 'SHR SHR)
     (cons 'AND AND)
     (cons 'BOR BOR)
     (cons 'XOR XOR)
     (cons 'IFE IFE)
     (cons 'IFN IFN)
     (cons 'IFG IFG) 
     (cons 'IFB IFB)))
  (let ([id (bitwise-and #xf hex)])
    (list-ref opcode-list id)))


; non-basic opcode format aaaaaaoooooo0000
; returns the symbol and op-func for a noon-basic hex instruction
(define (fetch-opcode-nonbasic hex)
  (define nonbasic-opcode-list
    (list
     (cons 'undef error)
     (cons 'JSR JSR)))
  (let ([id (bitwise-and #b111111
                         (arithmetic-shift hex -4))])
    (list-ref nonbasic-opcode-list id)))

; fill in the information needed by op-func to generate
; the final function to be called to execute the oplist
(define (build-call-from-oplist oplist)
  (let* ([varlist (instruction-var-list oplist)]
         [var-cycles (isr-size varlist)]
         [op-func (instruction-op-func oplist)])
    (op-func var-cycles (build-op-write (last varlist)))))

(define (in-between a v b)
  (and (<= a v)
       (<= v b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reg is the register being updated
; returns updated register updated such that 'Pa/'Pb are set to v
; and 'Paadr/'Pbadr are set to vadr
;
; SAMPLE set-param
; 
; (lambda (v vadr)
;   (reg-write (reg-write reg 'Pa v)
;              'Paadr
;              vadr))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-var var-id mem reg set-param)
  ;0x00-0x07: register
  (define (REG r-id)
    (display (list 'REG (reg-name r-id)))
    (set-param (reg-read reg r-id) 0))
  
  ;0x08-0x0f: [register]
  (define (DEREF-REG r-id)
    (display (list 'DEREF 'REG (reg-name r-id)))
    (let ([addr (reg-read reg r-id)])
      (set-param (memory-read mem addr) addr)))
  
  ;0x10-0x17: [next word + register]
  ; [next word + register]
  (define (DEREF-NW+ r-id)
    (display (list 'DEREF 'NW+ (reg-name r-id)))
    (let* ([nw (memory-read mem (reg-read reg 'PC))]
           [r-val (reg-read reg r-id)]
           [calc-addr (+ nw r-val)])
      (printf "calc-addr:~x\n" calc-addr)
      (reg-inc (set-param (memory-read mem calc-addr) calc-addr)
               'PC)))
  
  ;after reg-inc, sp-val is no longer valid in this context\
  ;0x18: POP / [SP++]
  (define (POP)
    (display (list 'POP))
    (let ([sp (reg-read reg 'SP)])
      (reg-inc (set-param (memory-read mem sp) sp)
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
      (reg-dec (set-param (memory-read mem sp-val) sp-val)
               'SP)))
  ;0x1b: SP
  (define (SP)
    (REG 'SP))
  
  ;0x1c: PC
  (define (PC)
    (REG 'PC))
  
  ;0x1d: O
  (define (O)
    (REG 'O))
  
  ;0x1e: [next word]
  (define (DEREF-NW)
    (display (list 'DEREF 'NW))
    (let ([adr (memory-read mem (reg-read reg 'PC))])
      (reg-inc (set-param (memory-read mem adr) adr)
               'PC)))
  
  ;0x1f: next word
  (define (NW)
    (display (list 'NW))
    (let ([pc (reg-read reg 'PC)])
      (reg-inc (set-param (memory-read mem pc) 0)
               'PC)))
  
  ;0x20-0x3f: literal 0x00-0x1f
  (define (LIT val)
    (display (list val))
    (set-param val 0))
  
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
    [else (error "invalid Value")]))

(define (parse-vars oplist ev-count mem reg)
  (let ([varlist (instruction-var-list oplist)])
    (define (reg-update var r raddr str)
      (update-var var
                  mem 
                  reg 
                  (lambda (v vadr)
                    (printf str v vadr)
                    (reg-write (reg-write reg r v)
                               raddr
                               vadr))))
    ;#b0000001111110000
    ; reg-update-a -> reg
    (define (reg-update-a)
      (reg-update (last varlist)
                  'Pa
                  'Paadr
                  "Pa:~x Paadr:~x\n"))
    
    ;#b1111110000000000
    ; reg-update-b -> reg
    (define (reg-update-b)
      (reg-update (first varlist)
                  'Pb
                  'Pbadr
                  "Pb:~x Pbadr:~x\n"))
    
    (case ev-count
      [(2)
       ;update registers with new value for Pa
       (parse-vars oplist
                   (- ev-count 1)
                   mem 
                   (reg-update-a))]
      [(1)
       ;execute the oplist with new value for Pb
       ((build-call-from-oplist oplist) mem
                                        (reg-update-b))])))

; execute the instruction located at 'PC
; 'PC will end up pointing to the next instruction
; step-cpu -> (mem . reg)
(define (step-cpu mem reg)
  (let* ([oplist (read-next-instruction mem reg)]
         [var-count (length (instruction-var-list oplist))])
    (printf "\n~s\n" (instruction-opcode oplist))
    (parse-vars oplist
                var-count ; number of params
                mem
                (reg-inc reg 'PC))))

(define (calc-instruction-size mem adr)
  (isr-size (instruction-var-list (hex->instruction (memory-read mem adr)))))