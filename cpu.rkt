#lang racket

;0x00-0x07: register (A, B, C, X, Y, Z, I or J, in that order)
(struct registers (A B C X Y Z I J SP PC O Pa Pb Paadr Pbadr CLK)
  #:transparent)

(define Reg (registers 0 0 0 0 0 0 0 0 #x10000 0 0 0 0 0 0 0))

(define (reg-name n)
  (case n
    [(0) 'A]
    [(1) 'B]
    [(2) 'C]
    [(3) 'X]
    [(4) 'Y]
    [(5) 'Z]
    [(6) 'I]
    [(7) 'J]
    [(8) 'SP]
    [(9) 'PC]
    [(10) 'O]
    [(11) 'Pa]
    [(12) 'Pb]
    [(13) 'Paadr]
    [(14) 'Pbadr]
    [(15) 'CLK]
    [else (error "Invalid register")]))

(define (reg-id s)
  (cond
    [(eq? 'A s) 0]
    [(eq? 'B s) 1]
    [(eq? 'C s) 2]
    [(eq? 'X s) 3]
    [(eq? 'Y s) 4]
    [(eq? 'Z s) 5]
    [(eq? 'I s) 6]
    [(eq? 'J s) 7]
    [(eq? 'SP s) 8]
    [(eq? 'PC s) 9]
    [(eq? 'O s) 10]
    [(eq? 'Pa s) 11]
    [(eq? 'Pb s) 12]
    [(eq? 'Paadr s) 13]
    [(eq? 'Pbadr s) 14]
    [(eq? 'CLK s) 15]))

(define (A++ regs)
  (let ([a (registers-A regs)])
    (struct-copy registers regs [A (+ a 1)])))

(define (set-a regs val)
  (struct-copy registers regs [A val]))

;(defin (set-reg regs r val)
;  (case r
;    [(0) (define r-name (A)]
;    [(0) (struct-copy registers regs [A val])]

;(A++ (set-a regs 10))

;(define (upd regs R val)
;  (struct-copy registers regs [(R) val]))

(define Mem (make-immutable-hash))
;(define Reg (make-immutable-hash))

;(define (reg-valid? r)
;  (member r ('A 'B 'C 'D 'X 'Y 'Z 'I 'J 'SP 'PC 'O)))

(define (reg-read reg r)
  (if (symbol? r)
      (reg-read reg (reg-id r))
      (case r
        [(0) (registers-A reg)]
        [(1) (registers-B reg)]
        [(2) (registers-C reg)]
        [(3) (registers-X reg)]
        [(4) (registers-Y reg)]
        [(5) (registers-Z reg)]
        [(6) (registers-I reg)]
        [(7) (registers-J reg)]
        [(8) (registers-SP reg)]
        [(9) (registers-PC reg)]
        [(10) (registers-O reg)]
        [(11) (registers-Pa reg)]
        [(12) (registers-Pb reg)]
        [(13) (registers-Paadr reg)]
        [(14) (registers-Pbadr reg)]
        [(15) (registers-CLK reg)]
        [else (error "Invalid register")])))

(define (reg-write reg r val)
  (if (symbol? r)
      (reg-write reg (reg-id r) val)
      (let ([mask-val (bitwise-and val #xffff)])
        (case r
          [(0) (struct-copy registers reg [A mask-val])]
          [(1) (struct-copy registers reg [B mask-val])]
          [(2) (struct-copy registers reg [C mask-val])]
          [(3) (struct-copy registers reg [X mask-val])]
          [(4) (struct-copy registers reg [Y mask-val])]
          [(5) (struct-copy registers reg [Z mask-val])]
          [(6) (struct-copy registers reg [I mask-val])]
          [(7) (struct-copy registers reg [J mask-val])]
          [(8) (struct-copy registers reg [SP mask-val])]
          [(9) (struct-copy registers reg [PC mask-val])]
          [(10) (struct-copy registers reg [O mask-val])]
          [(11) (struct-copy registers reg [Pa mask-val])]
          [(12) (struct-copy registers reg [Pb mask-val])]
          [(13) (struct-copy registers reg [Paadr mask-val])]
          [(14) (struct-copy registers reg [Pbadr mask-val])]
          [(15) (struct-copy registers reg [CLK mask-val])]
          [else (error "Invalid register")]))))

(define (memory-valid? adr)
  (and (<= 0 adr)
       (> #x10000 adr)))

(define (memory-read memory adr)
  (if (memory-valid? adr)
      (hash-ref memory adr 0)
      ((error "RAM out of bounds" adr)
       0)))

(define (memory-write memory adr val)
  (let ([mask-val (bitwise-and val #xffff)])
    (if (memory-valid? adr)
        (hash-set memory adr mask-val)
        ((error "RAM out of bounds")
         memory))))

(define (memory-fill memory startadr vals)
  (if (empty? vals)
      memory
      (memory-fill
       (memory-write memory startadr (first vals))
       (+ 1 startadr)
       (rest vals))))

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


(define (build-op-write hex reg)
  (let ([i (bitwise-bit-field hex 4 10)])
    (cond
      [(in-between  #x0 i  #x7) (build-op-reg-write i)]
      [(in-between  #x8 i  #xf) (build-op-mem-write)]
      [(in-between #x10 i #x17) (build-op-mem-write)]
      [(= #x18 i) (build-op-mem-write)]
      [(= #x19 i) (build-op-mem-write)]
      [(= #x1a i) (build-op-mem-write)]
      [(in-between #x1b i #x1d) (build-op-reg-write(- i 19))]
      [(= #x1e i) (build-op-mem-write)]
      [(= #x1f i) (build-op-lit-write)]
      [(in-between #x20 i #x3f) (build-op-lit-write)]
      
      [else (error "invalid Value")]
      )))


(define (build-op-mem-write)
  (lambda (val mem reg)
    (cons (memory-write mem
                        (reg-read reg 'Paadr)
                        val)
          reg)))

(define (build-op-reg-write r-id)
  (lambda (val mem reg)
    (cons mem
          (reg-write reg r-id val))))

(define (build-op-lit-write)
  (lambda (val mem reg)
    (cons mem
          reg)))

(define (var-size v)
  (cond
    [(in-between #x10 v #x17) 1]
    [(= #x1e v) 1]
    [(= #x1f v) 1]
    [else 0]))

(define (calc-clock-nonbasic hex)
  (let ([s (fetch-opcode-nonbasic hex)])
    (+ (isr-size hex)
       (cond 
         [(eq? 'JSR s) 1]))))

(define (calc-clock hex)
  (let ([s (fetch-opcode hex)])
    (if (eq? 'nonbasic s)
        (calc-clock-nonbasic hex)
        ; clock cycles are (isr size + isr cost)
        (+ (isr-size hex)
           (cond 
             [(eq? 'SET s) 0]
             [(eq? 'ADD s) 1]
             [(eq? 'SUB s) 1]
             [(eq? 'MUL s) 1]
             [(eq? 'DIV s) 2]
             [(eq? 'MOD s) 2]
             [(eq? 'SHL s) 1]
             [(eq? 'SHR s) 1]
             [(eq? 'AND s) 0]
             [(eq? 'BOR s) 0]
             [(eq? 'XOR s) 0]
             [(eq? 'IFE s) 1]
             [(eq? 'IFN s) 1]
             [(eq? 'IFG s) 1]
             [(eq? 'IFB s) 1])
           ))))

(define (clock-cycle reg clks)
  (let ([clk (reg-read reg 'CLK)])
    (reg-write reg
               'CLK
               (+ clk clks))))


(define (isr-size-nonbasic hex)
  (let ([vb (var-size (bitwise-bit-field hex 10 16))]
        [s (fetch-opcode-nonbasic hex)])
    (cond 
      [else (+ 1 vb)])))

(define (isr-size hex)
  (let ([va (var-size (bitwise-bit-field hex 4 10))]
        [vb (var-size (bitwise-bit-field hex 10 16))]
        [s (fetch-opcode hex)])
    (cond
      [(eq? 'nonbasic s) (isr-size-nonbasic hex)]
      [else (+ 1 va vb)])))

(define (skip-isr hex reg)
  (let* ([va (var-size (bitwise-bit-field hex 4 10))]
         [vb (var-size (bitwise-bit-field hex 10 16))]
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

; 0xc: IFE a, b - performs next instruction only if a==b
(define (IFE cycles mem reg w-func)
  (let ([pa (reg-read reg 'Pa)]
        [pb (reg-read reg 'Pb)]
        [hex (memory-read mem
                          (reg-read reg 'PC))])
    (if (= pa pb)
        (w-func 0 
                mem 
                (clock-cycle reg
                             cycles))
        (w-func 0 
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
        (w-func 0 
                mem 
                (clock-cycle (skip-isr hex reg)
                             (+ 1 cycles))) ; skip next instruction & values
        (w-func 0 
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
        (w-func 0 
                mem 
                (clock-cycle reg
                             cycles))
        (w-func 0 
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
        (w-func 0 
                mem 
                (clock-cycle (skip-isr hex reg)
                             (+ 1 cycles))) ; skip next instruction & values
        (w-func 0 
                mem 
                (clock-cycle reg
                             cycles)))))

;(define (PUSH mem reg set-param)
;  (let ([sp-val (- (reg-read reg 'SP) 1)])
;    (reg-dec (set-param (memory-read mem
;                                     sp-val)
;                        sp-val
;                        reg)
;             'SP)))

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
    (begin (display s)
           (cond
             [(eq? 'JSR s) (JSR cycles mem reg)]))))

(define (call-hex hex mem reg)
  (let ([w-func (build-op-write hex reg)]
        [s (fetch-opcode hex)]
        [cycles (calc-clock hex)])
    (begin (display s)
           (display "\n")
           (cond 
             [(eq? 'nonbasic s) (call-nonbasic-hex hex mem reg)]
             [(eq? 'SET s) (SET cycles mem reg w-func)]
             [(eq? 'ADD s) (ADD cycles mem reg w-func)]
             [(eq? 'SUB s) (SUB cycles mem reg w-func)]
             [(eq? 'MUL s) (MUL cycles mem reg w-func)]
             [(eq? 'DIV s) (DIV cycles mem reg w-func)]
             [(eq? 'MOD s) (MOD cycles mem reg w-func)]
             [(eq? 'SHL s) (SHL cycles mem reg w-func)]
             [(eq? 'SHR s) (SHR cycles mem reg w-func)]
             [(eq? 'AND s) (AND cycles mem reg w-func)]
             [(eq? 'BOR s) (BOR cycles mem reg w-func)]
             [(eq? 'XOR s) (XOR cycles mem reg w-func)]
             [(eq? 'IFE s) (IFE cycles mem reg (build-op-lit-write))]
             [(eq? 'IFN s) (IFN cycles mem reg (build-op-lit-write))]
             [(eq? 'IFG s) (IFG cycles mem reg (build-op-lit-write))]
             [(eq? 'IFB s) (IFB cycles mem reg (build-op-lit-write))]))))

(define (in-between a v b)
  (and (<= a v)
       (<= v b)))

; sample set-param
; reg is the register structure being updated
; returns a structure updated such that register 'Pa/'Pb are set to v
; lambda(v) (reg-write r (reg-id 'Pa) v) ; write parameter a
; lambda(v) (reg-write r (reg-id 'Pb) v) ; write parameter b

(define (reg-inc reg r-id)
  (reg-write reg r-id (+ (reg-read reg r-id) 1)))

(define (reg-dec reg r-id)
  (reg-write reg r-id (- (reg-read reg r-id) 1)))

;0x00-0x07: register
(define (REG r-id mem reg set-param)
  (set-param (reg-read reg 
                       r-id)
             0
             reg))


;0x08-0x0f: [register]
(define (DEREF-REG r-id mem reg set-param)
  (let ([addr (reg-read reg r-id)])
    (set-param (memory-read mem 
                            addr)
               addr
               reg)))

;0x10-0x17: [next word + register]
; [next word + register]
(define (DEREF-NW+ r-id mem reg set-param)
  (let* ([nw (memory-read mem
                          (reg-read reg 'PC))]
         [r-val (reg-read reg r-id)]
         [calc-addr (+ nw r-val)])
    (begin (display "calc-addr")
           (display calc-addr)
           (display "\n")
           (reg-inc (set-param (memory-read mem
                                            calc-addr)
                               calc-addr
                               reg)
                    'PC))))

;after reg-inc, sp-val is no longer valid in this context\
;0x18: POP / [SP++]
(define (POP mem reg set-param)
  (let ([sp (reg-read reg 'SP)])
    (reg-inc (set-param (memory-read mem sp)
                        sp
                        reg)
             'SP)))

;0x19: PEEK / [SP]
(define (PEEK mem reg set-param)
  (DEREF-REG 'SP mem reg set-param))

; fake the fact that
; sp-val = [--SP]
(define (PUSH mem reg set-param)
  (let ([sp-val (- (reg-read reg 'SP) 1)])
    (reg-dec (set-param (memory-read mem
                                     sp-val)
                        sp-val
                        reg)
             'SP)))

(define (SP mem reg set-param)
  (REG 'SP mem reg set-param))

(define (PC mem reg set-param)
  (REG 'PC mem reg set-param))

(define (O mem reg set-param)
  (REG 'O mem reg set-param))

(define (DEREF-NW mem reg set-param)
  (let ([adr (memory-read mem 
                          (reg-read reg 'PC))])
    (reg-inc (set-param (memory-read mem
                                     adr)
                        adr
                        reg)
             'PC)))

(define (NW mem reg set-param)
  (let ([pc (reg-read reg 'PC)])
    (reg-inc (set-param (memory-read mem pc)
                        0
                        reg)
             'PC)))


(define (fetch-helper s f)
  (lambda (hex mem reg set-param)
    (let ([i (bitwise-bit-field hex s f)])
      (cond
        [(in-between  #x0 i  #x7) (begin (display (list 'REG (reg-name i)))
                                         (REG i mem reg set-param))]
        [(in-between  #x8 i  #xf) (begin (display (list 'DEREF 'REG (reg-name (- i #x8))))
                                         (DEREF-REG (- i #x8) mem reg set-param))]
        [(in-between #x10 i #x17) (begin (display (list 'DEREF 'NW+ (reg-name (- i #x10))))
                                         (DEREF-NW+ (- i #x10) mem reg set-param))]
        [(= #x18 i) (begin (display (list 'POP))
                           (POP mem reg set-param))]
        [(= #x19 i) (begin (display (list 'PEEK))
                           (PEEK mem reg set-param))]
        [(= #x1a i) (begin (display (list 'PUSH))
                           (PUSH mem reg set-param))]
        [(in-between #x1b i #x1d) (begin (display (list 'REG (reg-name (- i 19))))
                                         (REG (- i 19) mem reg set-param))]
        [(= #x1e i) (begin (display (list 'DEREF 'NW))
                           (DEREF-NW mem reg set-param))]
        [(= #x1f i) (begin (display (list 'NW))
                           (NW mem reg set-param))]
        [(in-between #x20 i #x3f) (begin (display (list (- i #x20)))
                                         (set-param (- i #x20)
                                                    0
                                                    reg))]
        
        [else (error "invalid Value")]
        ))))

;(define (fetch-helper s f)
;  (lambda (hex mem reg set-param)
;    (begin 
;      (display "fetch-helper ")
;      (display reg)
;      (display "\n")
;      (let ([i (bitwise-bit-field hex s f)])
;        (cond
;          [(in-between  #x0 i  #x7) (REG i mem reg set-param)]
;          [(in-between  #x8 i  #xf) (DEREF-REG i mem reg set-param)]
;          [(in-between #x10 i #x17) (DEREF-NW+ i mem reg set-param)]
;          [(= #x18 i) (POP mem reg set-param)]
;          [(= #x19 i) (PEEK mem reg set-param)]
;          [(= #x1a i) (PUSH mem reg set-param)]
;          [(in-between #x1b i #x1d) (REG (- i 19) mem reg set-param)]
;          [(= #x1e i) (DEREF-NW mem reg set-param)]
;          [(= #x1f i) (NW mem reg set-param)]
;          [(in-between #x20 i #x3f) (set-param (- i #x20)
;                                               reg)]
;          
;          [else (error "invalid Value")]
;          )))))

(define (reg-update hex ev-count mem reg)
  (define (Pa-helper v vadr reg)
    (begin (display "Pa:")
           (display v)
           (display " Paadr:")
           (display vadr)
           (display "\n")
           (reg-write (reg-write reg 'Pa v)
                      'Paadr
                      vadr)))
  (define (Pb-helper v vadr reg)
    (begin (display "Pb:")
           (display v)
           (display " Pbadr:")
           (display vadr)
           (display "\n")
           (reg-write (reg-write reg 'Pb v)
                      'Pbadr
                      vadr)))
  ;#b0000001111110000
  (define fetch-a (fetch-helper 4 10 ))
  ;#b1111110000000000
  (define fetch-b (fetch-helper 10 16 ))
  (begin 
    ;(display "reg-update: ")
    ;(display reg)
    ;(display "\n")
    (case ev-count
      [(2)
       ;update registers with new value for Pa based on the hex
       
       (let ([new-reg (fetch-a hex mem reg Pa-helper)])
         (reg-update hex
                     (- ev-count 1)
                     mem 
                     new-reg))]
      [(1) 
       (let ([new-reg (fetch-b hex mem reg Pb-helper)])
         (call-hex hex
                   mem
                   new-reg))])))

(define (parse-pc mem reg)
  (let* ([pc-val (reg-read reg 'PC)]
         [hex (memory-read mem pc-val)]
         [ev-count (if (eq? (fetch-opcode hex) 'nonbasic)
                       1
                       2)])
    ;(if (>= pc-val 8)
    ;(cons mem reg)
    (reg-update hex
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