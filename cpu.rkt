#lang racket

(require "registers.rkt") ; new list based registers
(require "memory.rkt")
(require "params.rkt")
(require "util.rkt")


(provide step-cpu
         calc-instruction-size)

(struct instruction
  (opcode
   op-func
   var-list
   basic?))

; 0x1: SET a, b - sets a to b
(define (SET w-func)
  (define op-cycles 1)
  (lambda (mem reg)
    (let ([val (reg-read reg 'Pb)])
      (w-func val
              mem
              (clock-cycle reg
                           op-cycles)))))

; 0x2: ADD a, b - sets a to a+b, sets O to 0x0001 if there's an overflow, 0x0 otherwise
(define (ADD w-func)
  (define op-cycles 2)
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
(define (SUB w-func)
  (define op-cycles 2)
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
(define (MUL w-func)
  (define op-cycles 2)
  (lambda (mem reg)
    (let* ([val (* (reg-read reg 'Pa)
                   (reg-read reg 'Pb))]
           [o-val (arithmetic-shift val -16)])
      (w-func val 
              mem 
              (clock-cycle (reg-write reg 'O o-val)
                           op-cycles)))))

; 0x5: DIV a, b - sets a to a/b, sets O to ((a<<16)/b)&0xffff. if b==0, sets a and O to 0 instead.
(define (DIV w-func)
  (define op-cycles 3)
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
(define (MOD w-func)
  (define op-cycles 3)
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
(define (SHL w-func)
  (define op-cycles 2)
  (lambda (mem reg)
    (let* ([val (arithmetic-shift (reg-read reg 'Pa)
                                  (reg-read reg 'Pb))]
           [o-val (arithmetic-shift val -16)])
      (w-func val 
              mem 
              (clock-cycle (reg-write reg 'O o-val)
                           op-cycles)))))

; 0x8: SHR a, b - sets a to a>>b, sets O to ((a<<16)>>b)&0xffff
(define (SHR w-func)
  (define op-cycles 2)
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
(define (AND w-func)
  (define op-cycles 1)
  (lambda (mem reg)
    (let* ([val (bitwise-and (reg-read reg 'Pa)
                             (reg-read reg 'Pb))])
      (w-func val
              mem
              (clock-cycle reg
                           op-cycles)))))

; 0xa: BOR a, b - sets a to a|b
(define (BOR w-func)
  (define op-cycles 1)
  (lambda (mem reg)
    (let* ([val (bitwise-ior (reg-read reg 'Pa)
                             (reg-read reg 'Pb))])
      (w-func val
              mem
              (clock-cycle reg
                           op-cycles)))))
; 0xb: XOR a, b - sets a to a^b
(define (XOR w-func)
  (define op-cycles 1)
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
(define (IFE w-func)
  (define op-cycles 2)
  (lambda (mem reg)
    (let ([pa (reg-read reg 'Pa)]
          [pb (reg-read reg 'Pb)]
          [n-hex (read-next-hex mem reg)])
      (if (= pa pb)
          (cons mem 
                (clock-cycle reg
                             op-cycles))
          (cons mem 
                (clock-cycle (skip-isr reg n-hex)
                             (+ 1 op-cycles))))))) ; skip next instruction & values
; 0xd: IFN a, b - performs next instruction only if a!=b
(define (IFN w-func)
  (define op-cycles 2)
  (lambda (mem reg)
    (let ([pa (reg-read reg 'Pa)]
          [pb (reg-read reg 'Pb)]
          [n-hex (read-next-hex mem reg)])
      (if (= pa pb)
          (cons mem 
                (clock-cycle (skip-isr reg n-hex)
                             (+ 1 op-cycles))) ; skip next instruction & values
          (cons mem  
                (clock-cycle reg
                             op-cycles))))))
; 0xe: IFG a, b - performs next instruction only if a>b
(define (IFG w-func)
  (define op-cycles 2)
  (lambda (mem reg)
    (let ([pa (reg-read reg 'Pa)]
          [pb (reg-read reg 'Pb)]
          [n-hex (read-next-hex mem reg)])
      (if (> pa pb)
          (cons mem  
                (clock-cycle reg
                             op-cycles))
          (cons mem  
                (clock-cycle (skip-isr reg n-hex)
                             (+ 1 op-cycles))))))) ; skip next instruction & values
; 0xf: IFB a, b - performs next instruction only if (a&b)!=0
(define (IFB w-func)
  (define op-cycles 2)
  (lambda (mem reg)
    (let ([val (bitwise-and (reg-read reg 'Pa)
                            (reg-read reg 'Pb))]
          [n-hex (read-next-hex mem reg)])
      (if (= val 0)
          (cons mem  
                (clock-cycle (skip-isr reg n-hex)
                             (+ 1 op-cycles))) ; skip next instruction & values
          (cons mem 
                (clock-cycle reg
                             op-cycles))))))


; 0x01 JSR a - pushes the address of the next instruction to the stack, then sets PC to a
; actually used Pb for simplicity's sake
(define (JSR w-func)
  (define op-cycles 2)
  (lambda (mem reg)
    (let* ([new-reg (reg-dec reg 'SP)]
           [sp (reg-read new-reg 'SP)]
           [pc (reg-read new-reg 'PC)]
           [pb (reg-read new-reg 'Pb)])
      (cons (memory-write mem sp pc)
            (clock-cycle (reg-write new-reg 'PC pb)
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

; returns the appropriate function to write to the Value
(define (build-op-write var-id)
  (third (list-ref param-list var-id)))

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


(define (read-next-hex mem reg)
  (let ([pc-val (reg-read reg 'PC)])
    (memory-read mem pc-val)))

; returns the instruction at 'PC
; does not increment 'PC
; read-next-instruction -> instruction
(define (read-next-instruction mem reg)
  (let ([hex (read-next-hex mem reg)])
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
                    (second (list-ref param-list var-id)))
                  varlist)))

; increment the 'PC to the start of the next instruction
(define (skip-isr reg hex)
  (reg-add reg 'PC (calc-instruction-size hex)))
;(reg-write reg 
;               'PC
;               (+ (reg-read reg 'PC) (isr-size varlist)))))

; fill in the information needed by op-func to generate
; the final function to be called to execute the oplist
(define (build-call-from-oplist oplist)
  (let ([varlist (instruction-var-list oplist)]
        [op-func (instruction-op-func oplist)])
    (op-func (build-op-write (last varlist)))))

(define (update-var var-id)
  (first (list-ref param-list var-id)))

(define (parse-vars oplist ev-count mem reg)
  (let ([varlist (instruction-var-list oplist)])
    (define (reg-update var r radr)
      ((update-var var) mem 
                        reg 
                        (lambda (reg v vadr)
                          (printf "~a:~x ~a:~x\n" r v radr vadr)
                          (reg-write (reg-write reg r v)
                                     radr
                                     vadr))))
    ;#b0000001111110000
    ; reg-update-a -> reg
    (define (reg-update-a)
      (reg-update (last varlist)
                  'Pa
                  'Paadr))
    
    ;#b1111110000000000
    ; reg-update-b -> reg
    (define (reg-update-b)
      (reg-update (first varlist)
                  'Pb
                  'Pbadr))
    
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

(define (calc-instruction-size hex)
  (isr-size (instruction-var-list (hex->instruction hex))))