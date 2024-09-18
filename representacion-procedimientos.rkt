#lang eopl

; constructores
(define cir_simple
    (lambda (c1 c2 ch)
        (lambda(s)
        (cond
        [(= s 0) 'cir_simple]
        [(= s 1) c1]
        [(= s 2) c2]
        [(= s 3) ch]
        [else (eopl:error 'cir_simple "Argumento no válido")]))))

(define cir_comp
    (lambda (cir1 cir2 c1 c2)
        (lambda(s)
        (cond
        [(= s 0) 'cir_comp]
        [(= s 1) cir1]
        [(= s 2) cir2]
        [(= s 3) c1]
        [(= s 4) c2]
        [else (eopl:error 'cir_comp "Argumento no válido")]))))

(define prim_chip
    (lambda (chip_prim)
        (lambda(s)
        (cond
        [(= s 0) 'prim_chip]
        [(= s 1) chip_prim]
        [else (eopl:error 'prim_chip "Argumento no válido")]))))

(define comp_chip
    (lambda (in out cir)
        (lambda (s)
        (cond
        [(= s 0) 'comp_chip]
        [(= s 1) in]
        [(= s 2) out]
        [(= s 3) cir]
        [else (eopl:error 'comp_chip "Argumento no válido")]))))

(define prim_or
    (lambda ()
        (lambda(s)
        (cond
        [(= s 0) 'chip-or]))))

(define prim_and
    (lambda()
        (lambda(s)
        (cond
        [(= s 0) 'chip-and]))))

(define prim_not
    (lambda()
        (lambda(s)
        (cond
        [(= s 0) 'chip-not]))))

(define prim_xor
    (lambda()
        (lambda(s)
        (cond
        [(= s 0) 'chip-xor]))))

(define prim_nand
    (lambda()
        (lambda(s)
        (cond
        [(= s 0) 'chip-nand]))))

(define prim_nor
    (lambda()
        (lambda(s)
        (cond
        [(= s 0) 'chip-nor]))))

(define prim_xnor
    (lambda()
        (lambda(s)
        (cond
        [(= s 0) 'chip-xnor]))))


; predicados
(define cir_simple?
    (lambda (x)
        (eq? (x 0) 'cir_simple)))

(define cir_comp?
    (lambda (x)
        (eq? (x 0) 'cir_comp)))

(define prim_chip?
    (lambda (x)
        (eq? (x 0) 'prim_chip)))

(define comp_chip?
    (lambda (x)
        (eq? (x 0) 'comp_chip)))

(define prim_or?
    (lambda (x)
        (eq? (x 0) 'prim_or)))

(define prim_and?
    (lambda (x)
        (eq? (x 0) 'prim_and)))

(define prim_not?
    (lambda (x)
        (eq? (x 0) 'prim_not)))

(define prim_xor? 
    (lambda (x)
        (eq? (x 0) 'prim_xor)))

(define prim_nand?
    (lambda (x)
        (eq? (x 0) 'prim_nand)))

(define prim_nor? 
    (lambda (x)
        (eq? (x 0) 'prim_nor)))

(define prim_xnor?
    (lambda (x)
        (eq? (x 0) 'prim_xnor)))


; extractores

(define cir_simple->c1
    (lambda (x)
        (x 1)))

(define cir_simple->c2
    (lambda (x)
        (x 2)))

(define cir_simple->ch
    (lambda (x)
        (x 3)))

(define cir_comp->cir1
    (lambda (x)
        (x 1)))

(define cir_comp->cir2
    (lambda (x)
        (x 2)))

(define cir_comp->c1
    (lambda (x)
        (x 3)))

(define cir_comp->c2
    (lambda (x)
        (x 4)))

(define prim_chip->chip_prim
    (lambda (x)
        (x 1)))

(define comp_chip->in
    (lambda (x)
        (x 1)))

(define comp_chip->out
    (lambda (x)
        (x 2)))

(define comp_chip->cir
    (lambda (x)
        (x 3)))

;Area del programador

(define chip1
  (comp_chip
  '(INA INB INC IND)
  '(OUTA)
  (cir_comp
    (cir_simple '(a b) '(e)
      (prim_chip (prim_and)))
    (list
      (cir_simple '(c d) '(f)
        (prim_chip (prim_and)))
      (cir_simple '(e f) '(g)
        (prim_chip (prim_or))))
  '(a b c d)
  '(g))))

(define circuito1
  (cir_comp
    (cir_simple '(a b) '(e)
      (prim_chip (prim_and)))
    (list
      (cir_simple '(c d) '(f)
        (prim_chip (prim_and)))
      (cir_simple '(e f) '(g)
        (prim_chip (prim_or))))
  '(a b c d)
  '(g)
  )
)