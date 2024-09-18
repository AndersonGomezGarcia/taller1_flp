#lang eopl
; Integrantes: 
;Anderson Gomez -2266242
;Nicolas Rodriguez - 2266071
;Michael Rodriguez -2266193
;Jhon Alexis Ruiz - 2266014

; Constructores
(define cir_simple
    (lambda (c1 c2 ch)
        (list 'cir_simple c1 c2 ch)))

(define cir_comp
    (lambda (cir1 cir2 c1 c2)
        (list 'cir_comp cir1 cir2 c1 c2)))

(define  prim_chip
    (lambda (chip_prim)
        (list 'prim-chip chip_prim)))

(define comp_chip
    (lambda (in out cir1)
        (list 'comp-chip in out cir1)))

(define prim_or
    (lambda ()
        (list 'chip-or)))

(define prim_and
    (lambda ()
        (list 'chip-and)))

(define prim_not
    (lambda ()
        (list 'chip-not)))

(define prim_xor
    (lambda ()
        (list 'chip-xor)))

(define prim_nand
    (lambda ()
        (list 'chip-nand)))

(define prim_nor
    (lambda ()
        (list 'chip-nor)))

(define prim_xnor 
    (lambda ()
        (list 'chip-xnor)))


; Predicados
(define cir_simple?
    (lambda (x)
        (eq? (car x) 'cir_simple)))

(define cir_comp?
    (lambda (x)
        (eq? (car x) 'cir_comp)))

(define prim_chip?
    (lambda (x)
        (eq? (car x) 'prim-chip)))

(define comp_chip? 
    (lambda (x)
        (eq? (car x) 'comp-chip)))
    
(define prim_or?
    (lambda (x)
        (eq? (car x) 'chip-or)))

(define prim_and?
    (lambda (x)
        (eq? (car x) 'chip-and)))

(define prim_not?
    (lambda (x)
        (eq? (car x) 'chip-not)))

(define prim_xor?
    (lambda (x)
        (eq? (car x) 'chip-xor)))

(define prim_nand? 
    (lambda (x)
        (eq? (car x) 'chip-nand)))

(define prim_nor? 
    (lambda (x)
        (eq? (car x) 'chip-nor)))

(define prim_xnor?
    (lambda (x)
        (eq? (car x) 'chip-xnor)))


; Extractores
(define cir_simple->c1
    (lambda (x)
        (cadr x)))

(define cir_simple->c2
    (lambda (x)
        (caddr x)))

(define cir_simple->ch
    (lambda (x)
        (cadddr x)))

(define cir_comp->cir1
    (lambda (x)
        (cadr x)))

(define cir_comp->cir2
    (lambda (x)
        (caddr x)))

(define cir_comp->c1
    (lambda (x)
        (cadddr x)))

(define cir_comp->c2
  (lambda (x)
    (car (cddddr x))))


(define prim_chip->chip_prim
    (lambda (x)
        (cadr x)))

(define comp_chip->in
    (lambda (x)
        (cadr x)))

(define comp_chip->out
    (lambda (x)
        (caddr x)))

(define comp_chip->cir
    (lambda (x)
        (cadddr x)))

; Area del programador

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

