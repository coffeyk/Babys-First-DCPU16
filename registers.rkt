#lang racket

(provide (struct-out registers)
         reg-name
         reg-id
         reg-read
         reg-write
         reg-inc
         reg-dec)

;0x00-0x07: register (A, B, C, X, Y, Z, I or J, in that order)
(struct registers (A B C X Y Z I J SP PC O Pa Pb Paadr Pbadr CLK)
  #:prefab)

(define (reg-name n)
  (if (symbol? n)
      n
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
        [else (error "Invalid register" n)])))

(define (reg-id s)
  (case s
    ['A      0]
    ['B      1]
    ['C      2]
    ['X      3]
    ['Y      4]
    ['Z      5]
    ['I      6]
    ['J      7]
    ['SP     8]
    ['PC     9]
    ['O     10]
    ['Pa    11]
    ['Pb    12]
    ['Paadr 13]
    ['Pbadr 14]
    ['CLK   15]))

(define (reg-read reg r)
  (case (reg-name r)
    ['A     (registers-A reg)]
    ['B     (registers-B reg)]
    ['C     (registers-C reg)]
    ['X     (registers-X reg)]
    ['Y     (registers-Y reg)]
    ['Z     (registers-Z reg)]
    ['I     (registers-I reg)]
    ['J     (registers-J reg)]
    ['SP    (registers-SP reg)]
    ['PC    (registers-PC reg)]
    ['O     (registers-O reg)]
    ['Pa    (registers-Pa reg)]
    ['Pb    (registers-Pb reg)]
    ['Paadr (registers-Paadr reg)]
    ['Pbadr (registers-Pbadr reg)]
    ['CLK   (registers-CLK reg)]
    [else (error "Invalid register")]))

(define (reg-write reg r val)
  (let ([mask-val (bitwise-and val #xffff)])
    (case (reg-name r)
      ['A     (struct-copy registers reg [A mask-val])]
      ['B     (struct-copy registers reg [B mask-val])]
      ['C     (struct-copy registers reg [C mask-val])]
      ['X     (struct-copy registers reg [X mask-val])]
      ['Y     (struct-copy registers reg [Y mask-val])]
      ['Z     (struct-copy registers reg [Z mask-val])]
      ['I     (struct-copy registers reg [I mask-val])]
      ['J     (struct-copy registers reg [J mask-val])]
      ['SP    (struct-copy registers reg [SP mask-val])]
      ['PC    (struct-copy registers reg [PC mask-val])]
      ['O     (struct-copy registers reg [O  mask-val])]
      ['Pa    (struct-copy registers reg [Pa mask-val])]
      ['Pb    (struct-copy registers reg [Pb mask-val])]
      ['Paadr (struct-copy registers reg [Paadr mask-val])]
      ['Pbadr (struct-copy registers reg [Pbadr mask-val])]
      ['CLK   (struct-copy registers reg [CLK   mask-val])]
      [else (error "Invalid register")])))

(define (reg-inc reg r-id)
  (reg-write reg r-id (+ (reg-read reg r-id) 1)))

(define (reg-dec reg r-id)
  (reg-write reg r-id (- (reg-read reg r-id) 1)))