#lang racket

(require racket/cmdline)
(require racket/gui/base)
(require srfi/13)

(require "cpu.rkt")
(require "memory.rkt")
(require "registers.rkt")

(define rows 8)
;(define (string-pad str width [pad #\space])
;  (define field-width (min width (string-length str)))
;  (define lmargin (- width field-width))
;  (string-append (build-string lmargin (lambda (x) pad))
;                 str))
(define (hex-pad x)
  (string-pad (format "~x" x) 4 #\0))
(define (row-header key)
  (string-append (hex-pad (- key (remainder key rows))) ":"))


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

(define mem-canv-text (new text%))
(define ed-canv (new editor-canvas% [parent text-panel]
                     [min-width 400]
                     [min-height 500]
                     [style (list 'hide-hscroll)]
                     [editor mem-canv-text]))

; Show the frame by calling its show method
(send frame show #t)

(define emu% (class object%
               (init mem reg)
               
               (define current-mem mem)
               (define current-reg reg)
               
               (refresh)
               
               (super-new)
               
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
               
               
               (define/public (refresh)
                 (let ([memory-text (memory-pprint current-mem)]
                       [pc (reg-read current-reg 'PC)])
                   (send reg-text set-value (reg-pprint current-reg))
                   (send mem-canv-text erase)
                   (send mem-canv-text change-style (make-object style-delta% 'change-family 'modern))
                   (send mem-canv-text insert memory-text )
                   (let ([row-idx (string-contains memory-text (row-header pc))])
                     (if row-idx
                         (let ([offset (+ row-idx 6 (* (remainder pc 8)
                                                       5))])
                           (send mem-canv-text change-style (make-object style-delta% 'change-toggle-underline)
                                 offset (+ 4 offset)))
                         (void)))))
               
               (define/public (tick)
                 (define mem-reg (step-cpu current-mem current-reg))
                 (set! current-mem (car mem-reg))
                 (set! current-reg (cdr mem-reg))
                 (refresh))))

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

(define Reg (build-reg))
(define Mem (make-immutable-hash))

; fill memory starting at #x0 from program
;(memory-fill Mem 0 program)

;(define program-file (command-line #:args (filename) filename))
;(define in (open-input-file program-file)); #:mode 'binary))
;(memory-fill Mem 0 (port->bytes in))

; execute program until PC is static
;(run (cons (memory-fill Mem 0 program) Reg) -1)

(define myemu (new emu% 
                   [mem (memory-fill Mem 0 program)]
                   [reg Reg]))

(define cpu-timer (new timer% [notify-callback (lambda ()(send myemu tick))]
                       [interval #f]
                       [just-once? #f]))