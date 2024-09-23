#lang eopl
; Integrantes: 
;Anderson Gomez -2266242
;Nicolas Rodriguez - 2266071
;Michael Rodriguez -2266193
;Jhon Alexis Ruiz - 2266014

; Constructores
(define cir_simple
    (lambda (c1 c2 ch)
        (list 'cir_simple c1 c2 ch))) ; c1 y c2 hacen referencia a los cables y ch al chip

(define cir_comp
    (lambda (cir1 lcirs c1 c2)
        (list 'cir_comp cir1 lcirs c1 c2))); cir1 es un circuito, lcircs es una lista de circuitos y c1 y c2 son cables (input output)
        
(define  prim_chip
    (lambda (chip_prim)
        (list 'prim-chip chip_prim))) ; chip_prim es un chip primitivo

(define comp_chip
    (lambda (in out cir1)
        (list 'comp-chip in out cir1))) ; in y out son cables y cir1 es un circuito

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

(define cir_comp->lcircs
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

; Ejemplos circuitos

(define cir1
  (cir_comp
   (cir_simple '(x1 x2) '(y1 y2)
    (comp_chip
     '(IN1 IN2) ; Lista de cables de entrada
     '(OUT1 OUT2); Lista de cables de salida
     (cir_simple '(a b) '(c)
      (prim_chip (prim_nand)))))
   (list ; Lista de circuitos
    (cir_simple '(d e) '(f)
     (comp_chip ; Chip compuesto
      '(IN3 IN4)
      '(OUT3)
      (cir_simple '(g h) '(i)
       (prim_chip (prim_xor))))))
   '(x1 x2 d e); Lista de cables de entrada
   '(y1 y2 f) ; Lista de cables de salida
   )
)

(define cir2
  (cir_comp
   (cir_simple '(w1 w2) '(z1 z2) ; circuito simple (cir1)
    (comp_chip ; chip compuesto
     '(INX INY)
     '(OUTX OUTY)
     (cir_comp  ; circuito del chip compuesto
      (cir_simple '(k l) '(m)
        (prim_chip (prim_xor)))
      (list ; Lista de circuitos del cir_comp del chip_comp
       (cir_simple '(n o) '(p)
         (prim_chip (prim_or))))
      '(k l n o)
      '(m p))))
   (list ; lista de circuitos del cir_comp
    (cir_simple '(q r) '(s)
     (prim_chip (prim_and))))
   '(w1 w2 q r) ; cables de entrada
   '(z1 z2 s) ; cables de salida
   )
)

(define cir3
  (cir_comp
   (cir_simple '(u v) '(w) ; circuito simple
    (prim_chip (prim_nor))) ; chip primitivo
   (list ; lista de circuitos
    (cir_simple '(x y) '(z)
     (prim_chip (prim_xnor)))
    (cir_simple '(a b) '(c)
     (prim_chip (prim_or))))
   '(u v x y a b) ; cables de entrada
   '(w z c); cables de salida
   )
)

(define cir4
  (cir_simple '(p q) '(r) ; circuito simple
   (comp_chip ; chip compuesto
    '(IN1 IN2)
    '(OUT1)
    (cir_simple '(s t) '(u) ; circuito simple del chip_compuesto
     (prim_chip (prim_not))))))


; Ejemplos chips

(define chip1
  (comp_chip ; chip compuesto
   '(IN_A IN_B)
   '(OUT_A OUT_B)
   (cir_comp
    (cir_simple '(i1 i2) '(o1)
      (prim_chip (prim_and)))
    (list ; lista de circuitos
     (cir_simple '(i3 i4) '(o2)
       (prim_chip (prim_nor))))
    '(i1 i2 i3 i4); cables de entrada
    '(o1 o2)); cables de salida
    )
)

(define chip2
  (comp_chip
   '(IN_A IN_B IN_C) ; cables de entrada
   '(OUT_X OUT_Y OUT_Z) ; cables de salida
   (cir_comp ; circuito compuesto
    (cir_simple '(a b) '(x)
     (prim_chip (prim_xor)))
    (list ; lista de circuitos 
     (cir_simple '(c d) '(y)
      (prim_chip (prim_and)))
     (cir_simple '(e f) '(z)
      (comp_chip
       '(IN1 IN2)
       '(OUT1)
       (cir_comp
        (cir_simple '(g h) '(j)
         (prim_chip (prim_or)))
        (list ; lista de circuitos
         (cir_simple '(k l) '(m)
          (prim_chip (prim_not))))
        '(g h k l)
        '(j m))))) 
    '(a b c d e f) ; cables de entrada
    '(x y z); cables de salida
    )
    )
)
