#lang racket

(require racket/cmdline)
(require racket/gui/base)
(require srfi/13)

(require "util.rkt")
(require "cpu.rkt")
(require "memory.rkt")
(require "registers.rkt")

(define rows 8)

(define red-style (make-object style-delta%))
;(send red-style set-delta-foreground "RoyalBlue")
(send red-style set-delta-background "Tan")

(define tan-bg-style (make-object style-delta%))
(send tan-bg-style set-delta-background "Tan")


(define (screen-pprint mem)
  (define base-addr #x8000)
  (define cols 32)
  (define rows 16)
  
  (define (hex->char adr)
    (let ([hex (bitwise-and (memory-read mem adr)
                            #xff)])
      (if (eq? hex 0)
          " "
          (bytes->string/utf-8 (make-bytes 1 hex)))))
  
  
  (define (print-row row)
    (string-append* ""
                    (map hex->char
                         (build-list cols
                                     (lambda (x)
                                       (+ x
                                          (* row cols)
                                          base-addr))))))
  (string-join (map print-row
                    (build-list rows values))
               "\n"))

;(define (string-pad str width [pad #\space])
;  (define field-width (min width (string-length str)))
;  (define lmargin (- width field-width))
;  (string-append (build-string lmargin (lambda (x) pad))
;                 str))


; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Baby's First DCPU16"]))




(define buton-panel (new horizontal-panel% [parent frame]))

(define run-buton (new button% [parent buton-panel]
                       [label "Run"]
                       ; Callback procedure for a button click:
                       (callback (lambda (button event)
                                   (send cpu-timer start 100)))))
(define step-buton (new button% [parent buton-panel]
                        [label "Step"]
                        ; Callback procedure for a button click:
                        (callback (lambda (button event)
                                    (send cpu-timer start 0 #t)))))
(define halt-buton (new button% [parent buton-panel]
                        [label "Halt"]
                        ; Callback procedure for a button click:
                        (callback (lambda (button event)
                                    (send cpu-timer stop)))))

(define load-buton (new button% [parent buton-panel]
                        [label "LOAD"]
                        ; Callback procedure for a button click:
                        (callback (lambda (button event)
                                    (let ([fname (get-file)])
                                      (if fname
                                          (send myemu load-mem (port->bytes
                                                                (open-input-file fname)))
                                          (void)))))))

(define reset-buton (new button% [parent buton-panel]
                         [label "Reset"]
                         ; Callback procedure for a button click:
                         (callback (lambda (button event)
                                     (send cpu-timer stop)
                                     (send myemu reset-reg)))))

(define text-panel (new horizontal-panel% [parent frame]))
(define reg-text (new text-field% [parent text-panel]
                      [label "reg"]
                      [font (make-object font% 10 'modern)]
                      [min-width 100]
                      [min-height 500]))

(define mem-panel (new vertical-panel% [parent text-panel]))
(define mem-canv-text (new text%))
(define ed-canv (new editor-canvas% [parent mem-panel]
                     [min-width 450]
                     [min-height 300]
                     [style (list 'hide-hscroll)]
                     [editor mem-canv-text]))

(define screen-canv-text (new text%))
(define screen-canv (new editor-canvas% [parent mem-panel]
                         [min-width 250]
                         [min-height 300]
                         [style (list 'hide-hscroll)]
                         [editor screen-canv-text])) 

; Show the frame by calling its show method
(send frame show #t)

(define emu% (class object%
               (init mem reg)
               
               (define current-mem mem)
               (define current-reg reg)
               
               (refresh)
               
               (super-new)
               
               ; format-list
               ; ( style-delta% addr1 addr2 )
               ; ( style-delta2% addr3 )
               (define/private (memory-format pp-str format-list (cols 8))
                 (define (apply-style style addr)
                   (let* ([addr-header (row-header addr cols)]
                          [header-idx (string-contains pp-str addr-header)]
                          [addr-col (remainder addr cols)])
                     (if header-idx
                         (let* ([addr-str-len (string-length (hex-pad addr))]
                                [addr-str-start (+ header-idx
                                                   (string-length addr-header)
                                                   (* addr-col 
                                                      (+ addr-str-len 1)))]
                                [addr-str-end (+ addr-str-start
                                                 addr-str-len)])
                           (send mem-canv-text 
                                 change-style 
                                 style 
                                 addr-str-start 
                                 addr-str-end))
                         (void))))
                 (map (lambda (format-entry)
                        (let ([style (car format-entry)]
                              [addrs (cdr format-entry)])
                          (map (lambda (addr)
                                 (apply-style style addr))
                               addrs)))
                      format-list))
               
               
               (define/public (get-mem)
                 current-mem)
               
               (define/public (load-mem program)
                 (set! current-mem (memory-fill (make-immutable-hash) 0 program))
                 (refresh))
               
               
               (define/public (get-reg)
                 current-reg)
               
               (define/public (reset-reg)
                 (set! current-reg (build-reg))
                 (refresh))
               
               
               (define/public (refresh (mem-diff-list '()))
                 (let* ([memory-text (memory-pprint current-mem)]
                        [pc (reg-read current-reg 'PC)]
                        [sp (reg-read current-reg 'SP)]
                        [paadr (reg-read current-reg 'Paadr)]
                        [i-size (calc-instruction-size current-mem pc)])
                   (send reg-text set-value (reg-pprint current-reg))
                   (send mem-canv-text erase)
                   (send mem-canv-text change-style (make-object style-delta% 'change-family 'modern))
                   (send mem-canv-text insert memory-text )
                   (memory-format memory-text (list (list* (make-object style-delta% 'change-toggle-underline) 
                                                           (map (lambda (x)
                                                                  (+ x pc))
                                                                (build-list i-size values)))
                                                    (list* red-style mem-diff-list)))
                   (send screen-canv-text erase)
                   (send screen-canv-text change-style (make-object style-delta% 'change-family 'modern))
                   (send screen-canv-text insert (screen-pprint current-mem))))
               ;(let ([row-idx (string-contains memory-text (row-header pc))])
               ;  (if row-idx
               ;      (let ([offset (+ row-idx 6 (* (remainder pc 8)
               ;                                    5))])
               ;        (send mem-canv-text change-style (make-object style-delta% 'change-toggle-underline)
               ;              offset (+ 4 offset)))
               ;      (void)))))
               
               (define/public (tick)
                 (define mem-reg (step-cpu current-mem current-reg))
                 (define mem-dif-list (memory-diff current-mem (car mem-reg)))
                 (set! current-mem (car mem-reg))
                 (set! current-reg (cdr mem-reg))
                 (refresh mem-dif-list))))

; calls step-cpu until the 'PC stops changing
(define (run mem-reg oldpc)
  (let* ([mem (car mem-reg)]
         [reg (cdr mem-reg)]
         [pc (reg-read reg 'PC)])
    (if (= oldpc pc)
        mem-reg
        (begin (display "\n")
               (display reg)
               (display "\n")
               (display (memory-pprint mem))
               (display "\n")
               (run (step-cpu mem reg) pc)))))


;Sample ASM code to be loaded
(define program (list #x7c01 #x0030 #x7de1 #x1000 #x0020 #x7803 #x1000 #xc00d
                      #x7dc1 #x001a #xa861 #x7c01 #x2000 #x2161 #x2000 #x8463
                      #x806d #x7dc1 #x000d #x9031 #x7c10 #x0018 #x7dc1 #x001a
                      #x9037 #x61c1 #x7dc1 #x001a #x0000))
(define program-test (list #x7deb #x000e #x5eed #x7d04 #x0002 #x6255 #x7d02 #x0002 
                           #x3619 #x8503 #x0001 #x433e #x0001 #x01c1 #xdaee #xf121 
                           #x5124 #xe2a2))

(define program-display (list #x7DE1 #x8000 #x0041 #x7DE1 #x8001 #x0042 #x7DE1 #x8002 #x0043 #x7DE1 #x8003 #x0044))

(define Reg (build-reg))
(define Mem (make-immutable-hash))

; fill memory starting at #x0 from program
;(memory-fill Mem 0 program)

(define program-file (command-line #:args ((filename "")) filename))
;(display program-file)
;(define in (open-input-file program-file)); #:mode 'binary))
;(memory-fill Mem 0 (port->bytes in))
;(memory-fill Mem 0 (port->bytes (open-input-file program-file)))

; execute program until PC is static
;(run (cons (memory-fill Mem 0 program) Reg) -1)

(define myemu (new emu% 
                   [mem (memory-fill Mem 0 (if (eq? program-file "")
                                               program-display
                                               (port->bytes (open-input-file program-file))))]
                   [reg Reg]))

(define cpu-timer (new timer% [notify-callback (lambda ()(send myemu tick))]
                       [interval #f]
                       [just-once? #f]))