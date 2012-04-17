#lang racket

(require "util.rkt")
(require srfi/13)
(require srfi/14)
(require (planet dyoo/python-tokenizer:1:=0))

;;; TODO
;; define "dat" function to write literals
;;  - strings are manually null terminated
;; add basic literal addition
;;  - extend to +-*/ eventually?
;; actually generate hex for each line
;;


(define (clean-list lst)
  (remove* (list '()) lst))

(define lnum (box 0))
(define (increment-lnum)
  (set-box! lnum (+ 1 (unbox lnum))))

(define label-table (make-hash))
(define line-addrs (make-hash))

;(define s "                    SET [0x1000], 0x20       ; 7de1 1000 0020")
(define s "      :loop         SET [0x2000+I], [A]      ; 2161 2000")
(define comment-start (string-index s #\;))
;(string-tokenize s char-set:graphic 0 comment-start)

(define (string-nocomments s)
  (let ([end (string-index s #\;)])
    (if end
        (substring s 0 end)
        s)))
(define (string-tokenize-quotes s)
  (let loop ([s s]
             [out '()])
    (displayln s)
    (let ([split (string-index s #\,)])
      (if split
          (loop (string-drop s (+ 1 split)) (append out (list (string-take s split))))
          (append out (list s))))))


(define (string-tokenize-comment s )
  (let ([end (string-index s #\;)])
    (if end
        (string-tokenize s (char-set-delete char-set:graphic #\,) 0 end)
        (string-tokenize s (char-set-delete char-set:graphic #\,)))))

(define (arg-val arg)
  (let ([arg-sym (first arg)]
        [arg-args (cdr arg)])
    (apply arg-sym arg-args)))

(define (STR str)
  (list* -1
         (map  (lambda (c)
                 (bitwise-and (char->integer c)
                              #xffff))
               (string->list str))))

(define (REG reg)
  (list (bitwise-and reg
                     #b111111)))
(define (LIT val)
  (let ([val (bitwise-and #xffff
                          val)])
    (if (< val #x20)
        (list (+ val #x20))
        (list #x1f val))))

(define (DEREF arg)
  (let* ([arg-v (arg-val arg)]
         [arg-hex (car arg-v)]
         [arg-nw  (cdr arg-v)])
    (cond
      [(< arg-hex 8)
       (list (+ arg-hex #x8))]
      [(in-between #x10 arg-hex #x17) (list* arg-hex arg-nw)]
      [(= arg-hex #x1f)
       (list* #x1e arg-nw)]
      [else (error arg)])))

(define (arg-add arg1 arg2)
  (let* ([arg-a (arg-val arg1)]
         [arg-a-hex (first arg-a)]
         [arg-a-nw  (cdr arg-a)]
         (arg-b (arg-val arg2))
         [arg-b-hex (first arg-b)]
         [arg-b-nw  (cdr arg-b)])
    (if (> arg-a-hex arg-b-hex)
        (arg-add arg2 arg1)
        (if (< arg-a-hex 8)
            (cond [(= arg-b-hex #x1f)
                   (list* (+ arg-a-hex #x10) arg-b-nw)]
                  [(in-between #x20 arg-b-hex #x3f)
                   (list #x1f (- arg-a-hex #x20))]
                  [else (error "arg-add" (list arg1 arg2))])
            (cond [(= arg-a-hex arg-b-hex #x1f)
                   (list #x1f (bitwise-and #xffff
                                           (+ (car arg-a-nw) 
                                              (car arg-b-nw))))]
                  
                  [(in-between #x20 arg-b-hex #x3f)
                   (list #x1f (- arg-a-hex #x20))]
                  [else (error "arg-add" (list arg1 arg2))])))))

(define (label-ref str)
  (if (hash-has-key? label-table str)
      (LIT  (hash-ref line-addrs (hash-ref label-table str) #xffff));(hash-ref label-table str))
      (error "undefined label:" str)))

(define (POP)
  (list #x18))
(define (PEEK)
  (list #x19))
(define (PUSH)
  (list #x1a))

(define (label-def str)
  '())

(define (basic->hex base-hex)
  (lambda (args)
    (if (eq? (length args)
             2)
        (let* ([arg-a (arg-val (first args))]
               [arg-a-hex (car arg-a)]
               [arg-a-nw  (cdr arg-a)]
               [arg-b (arg-val (second args))]
               [arg-b-hex (car arg-b)]
               [arg-b-nw  (cdr arg-b)]
               [nws (clean-list (append arg-a-nw arg-b-nw))])
          (list* (bitwise-ior base-hex
                              (arithmetic-shift arg-a-hex 4)
                              (arithmetic-shift arg-b-hex 10))
                 nws))
        (error (format "incorrect number of operands: ~a" (length args))))))

(define (nonbasic->hex base-hex)
  (lambda (arg)
    (if (= (length arg)
           1)
        (let* ([arg-a (arg-val (first arg))]
               [arg-a-hex (car arg-a)]
               [arg-a-nw  (cdr arg-a)]
               [nws (clean-list arg-a-nw)])
          (list* (bitwise-ior (arithmetic-shift base-hex 4)
                              (arithmetic-shift arg-a-hex 10))
                 nws))
        (error "too many args"))))
(define opcode-list
  (list 
   (cons "SET" (basic->hex 1))
   (cons "ADD" (basic->hex 2)) 
   (cons "SUB" (basic->hex 3) )
   (cons "MUL" (basic->hex 4) )
   (cons "DIV" (basic->hex 5) )
   (cons "MOD" (basic->hex 6) )
   (cons "SHL" (basic->hex 7) )
   (cons "SHR" (basic->hex 8) )
   (cons "AND" (basic->hex 9) )
   (cons "BOR" (basic->hex 10) )
   (cons "XOR" (basic->hex 11) )
   (cons "IFE" (basic->hex 12) )
   (cons "IFN" (basic->hex 13) )
   (cons "IFG" (basic->hex 14) ) 
   (cons "IFB" (basic->hex 15) )))

(define nonbasicopcode-list
  (list
   (cons "JSR" (nonbasic->hex 1))))

(define (DAT args)
  (define (gather-args args nw-list)
    (if (empty? args)
        nw-list
        (let* ([arg-a (arg-val (first args))]
               [arg-a-hex (car arg-a)]
               [arg-a-nw  (cdr arg-a)]
               [nws (clean-list arg-a-nw)])
          (if (in-between #x20 arg-a-hex #x3f)
              (gather-args (rest args) (append nw-list (list (- arg-a-hex #x20))))
              (gather-args (rest args) (append nw-list nws))))))
  (gather-args args '()))

(define customopcode-list
  (list
   (cons "DAT" DAT)))

(define register-list
  '(("A" . 0)
    ("B" . 1)
    ("C" . 2) 
    ("X" . 3)
    ("Y" . 4)
    ("Z" . 5)
    ("I" . 6)
    ("J" . 7)
    ("SP" . #x1b)
    ("PC" . #x1c)
    ("O" .  #x1d)))

(define (string-drop-both token n)
  (string-drop-right (string-drop token n) n))

(define (process-arg token)
  (cond 
    ;String
    [(and (string=? (string-take token 1) "\"")
          (string=? (string-take-right token 1) "\""))
     (list STR (string-drop-both token 1))]
    ;Deref
    [(and (string=? (string-take token 1) "[")
          (string=? (string-take-right token 1) "]"))
     (let ([inner-token (string-drop-both token 1)])
       (list DEREF (process-arg inner-token)))]
    ;Register
    [(assoc (string-upcase token) register-list)
     (list REG (cdr (assoc (string-upcase token) register-list)))]
    ;PUSH
    [(string=? (string-downcase token)
               "push")
     (list PUSH)]
    ;POP
    [(string=? (string-downcase token)
               "pop")
     (list POP)]
    ;PEEK
    [(string=? (string-downcase token)
               "peek")
     (list PEEK)]
    ;Register + Constant
    [(string-index token #\+) 
     (let ([tokens (string-tokenize token (char-set-delete char-set:graphic #\+))])
       (list arg-add (process-arg (first tokens)) (process-arg (second tokens))))]
    ;Literal, base 10
    [(string->number token)
     (list LIT (string->number token))]
    ;Literal, hex
    [(and (>= (string-length token)
              2)
          (string=? (string-downcase (string-take token 2))
                    "0x"))
     (list LIT (string->number (string-drop token 2) 16))]
    ;assume label reference
    [else
     (list label-ref token)]))

(define (process-args tokens)
  (if (empty? tokens)
      '()
      (list* 
       (process-arg (first tokens))
       (process-args(rest tokens)))))

(define (process-customop tokens)
  (unless (empty? tokens)
    (let* ([op-str (string-upcase (first tokens))]
           [op (assoc op-str customopcode-list)])
      (if op 
          (list* (cdr op) (process-args (rest tokens)))
          (error (format "line ~a error: Undefined op '~a'. Ref: ~a" (unbox lnum) (first tokens) tokens))))))

(define (process-nonbasicop tokens)
  (unless (empty? tokens)
    (let* ([op-str (string-upcase (first tokens))]
           [op (assoc op-str nonbasicopcode-list)])
      (if op
          (list* (cdr op) (process-args (rest tokens)))
          (process-customop tokens)))))

(define (process-op tokens)
  (if (empty? tokens)
      '()
      (let* ([op-str (string-upcase (first tokens))]
             [op (assoc op-str opcode-list)])
        (increment-lnum)
        (if op
            (list* (cdr op) (process-args (rest tokens)))
            (process-nonbasicop tokens)))))

;;;;;;;;; Line format
;(:label) basic-op arg arg
;(:label) nonbasic-op arg


(define (process-file lines out)
  (define (process-line tokens out)
    (if (empty? tokens)
        (process-file (stream-rest lines) out)
        (let* ([op/label (first tokens)]
               [line-int (unbox lnum)])
          
          (if (eq? #\: (string-ref op/label 0))
              ;label
              (let ([label-name (string-drop op/label 1)])
                ; add the label's line number to the table
                (hash-set! label-table label-name line-int)
                (process-line (rest tokens)
                              (list* (list line-int label-def label-name) 
                                     out)))
              ;op
              (process-file (stream-rest lines)
                            (list* (list* line-int (process-op tokens)) out))))))
  (if (stream-empty? lines)
      out
      (let* ([line (string-trim (string-upcase (stream-first lines)))]
             [tokens (string-tokenize-comment line)])
        (process-line tokens
                      out))))
;(define line " A, 0x20, \"123,45\" ;world")
(define (process-file-str lines out)
  (define (fetch-args args-string)
    (define (f-deref tokens arg-list arg-builder)
      (if (empty? tokens)
          (error arg-list)
          (let* ([token (first tokens)]
                 [token-type (first token)]
                 [token-text (second token)]
                 [new-arg-builder (string-append arg-builder token-text)])
            (case token-type
              [(NAME NUMBER STRING) (f-deref (rest tokens)
                                             arg-list
                                             new-arg-builder)]
              [(OP) (cond
                      [(string=? token-text
                                 "+") (f-deref (rest tokens)
                                               arg-list
                                               new-arg-builder)]
                      [(string=? token-text
                                 "]") (f-literal (rest tokens) 
                                                 (append arg-list (list new-arg-builder))
                                                 #t)]
                      [else (error tokens)])]
              [(INDENT NL NEWLINE) (f-deref (rest tokens) 
                                            arg-list
                                            "")]
              [else (error  tokens)]))))
    
    (define (f-literal tokens arg-list comma?)
      (if (empty? tokens)
          arg-list
          (let* ([token (first tokens)]
                 [token-type (first token)]
                 [token-text (second token)])
                (case token-type
                  [(NAME NUMBER STRING) (if comma?
                                            (error "Expected comma" token)
                                            (f-literal (rest tokens) (append arg-list (list token-text)) #t))]
                  [(OP) (cond
                          [(string=? token-text
                                     ",") (if comma?
                                              (f-literal (rest tokens) arg-list #f)
                                              (error "Unexpected comma" token))]
                          [(string=? token-text
                                     ";") (if comma?
                                              arg-list
                                              (error "Expected opperand" token))]
                          [(string=? token-text
                                     "[") (if comma?
                                              (error "Expected comma" token)
                                              (f-deref (rest tokens) arg-list token-text))] 
                          [else (error tokens)])]
                  [(INDENT NL NEWLINE) (f-literal (rest tokens) arg-list comma?)]
                  [(ENDMARKER)  arg-list]
                  [else (error  tokens)]))))
    
    (define (args->tokens args-string)
      (begin0
        (call-with-exception-handler (lambda (e)
                                       (error (format "Line ~a: Unbalanced brackets?\n" (unbox lnum))))
                                     (lambda ()
                                       (sequence->list (generate-tokens (open-input-string args-string)))
                                       ))
        (void)))
    (f-literal (args->tokens args-string)
               '()
               #f))
  
  (define (process-line line out)
    (if (equal? line "")
        (process-file-str (stream-rest lines) out)
        ;try regeg label
        ;;sample results
        ;;(":stuff SET A, 0X30" "stuff" "SET A, 0X30" "SET" "A, 0X30")
        ;;(":stuff SET A, 0X30" ":stuff" "stuff" "SET A, 0X30" "SET" "A, 0X30")
        (let ([re-line (regexp-match #px"(:([^\\s]+)\\s*)?(([^\\s]+)\\s+(.+))?\\s*" line)])
          (if re-line
              (let ([label-name (third re-line)]
                    [line-int (unbox lnum)]
                    [op (fifth re-line)])
                ;; check if label exists
                ;; figure out op
                ;; generate op + args tokens, where args can be quoted
                (if label-name
                    (let ([op-args (if (fourth re-line)
                                       (string-trim-both (fourth re-line))
                                       "")])
                      (hash-set! label-table label-name line-int)
                      (process-line op-args
                                    (list* (list* line-int label-def label-name)
                                           out)))
                    (if op
                        (let* ([args (if (sixth re-line)
                                         (string-trim-both (sixth re-line))
                                         "")]
                               [new-out (if (string=? (string-take op 1)
                                                      ";")
                                            out
                                            (list* (list* line-int (process-op (list* op (fetch-args args)))) out))])
                          (process-file-str (stream-rest lines)
                                            new-out))
                        (process-file-str (stream-rest lines)
                                          out))))
              (error line)))))
  
  (if (stream-empty? lines)
      out
      (let* ([line (string-trim (stream-first lines))])
        (process-line line
                      out))))


(define (token->hex token)
  (let ([op (car token)]
        [args (cdr token)])
    ;(display token)
    (op args)))

(define (pprint hex)
  ;(display "\n =>")
  (for ([byte (in-list hex)])
    (printf "~a "(hex-pad byte)))
  (displayln ""))


(define (tokens->hex tokens)
  (define (helper tokens adr out)
    (if (empty? tokens)
        out
        (let* ([entry (first tokens)]
               [l-num (car entry)]
               [token (cdr entry)]
               [hex  (token->hex token)])
          (hash-set! line-addrs l-num adr)
          (helper (rest tokens) 
                  (+ adr
                     (length hex))
                  (append out hex)))))
  (helper tokens 0 (list)))

(define (fixed-point tokens old-hex)
  (let ([new-hex (tokens->hex tokens)])
    (if (equal? new-hex
                old-hex)
        new-hex
        (begin
          (fixed-point tokens new-hex)))))


(define (file->tokens in)
  (reverse (process-file-str (sequence->stream (in-lines in)) '())))

(define in (open-input-file "test6.asm"))
;(for ([line (in-lines in)])
;  (let ([tokens (string-tokenize-comment line)])
;    (unless (empty? tokens)
;      (displayln (process-line tokens)))))

(define tokens (file->tokens in))
;tokens

(pprint (fixed-point tokens ""))
;(for ([token (in-list tokens)])
;  (pprint(token->hex (cdr token))))