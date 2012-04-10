#lang racket

(require racket/cmdline)
(require racket/gui/base)

(require "cpu.rkt")
(require "memory.rkt")
(require "registers.rkt")
         




; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))


(define vert-panel (new horizontal-panel% [parent frame]))

(define reg-text (new text-field% [parent vert-panel]
                      [label "reg"]
                      [font (make-object font% 10 'modern)]
                      [min-width 100]
                      [min-height 500]))
(define mem-text (new text-field% [parent vert-panel]
                      [label "mem"]
                      [min-height 500]
                      [min-width 500]
                      [font (make-object font% 10 'modern)]))


; Show the frame by calling its show method
(send frame show #t)

(define emu% (class object%
               (init mem reg)
               
               (define current-mem mem)
               (define current-reg reg)
               (super-new)
               
               (define/public (get-mem)
                 current-mem)
               (define/public (get-reg)
                 current-reg)
               
               (define/public (tick)
                 (define mem-reg (step-cpu current-mem current-reg))
                 (set! current-mem (car mem-reg))
                 (set! current-reg (cdr mem-reg))
                 (send reg-text set-value (reg-pprint current-reg))
                 (send mem-text set-value (memory-pprint current-mem))
                 (display current-mem))))

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

(new timer% [notify-callback (lambda ()(send myemu tick))]
     [interval 100]
     [just-once? #f])