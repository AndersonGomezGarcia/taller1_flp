#lang eopl
; Integrantes: 
;Anderson Gomez -2266242
;Nicolas Rodriguez - 2266071
;Michael Rodriguez -2266193
;Jhon Alexis Ruiz - 2266014

(define-datatype circuito circuito?
    (cir_simple (c1 (list-of symbol?))
                (c2 (list-of symbol?))
                (ch chip?))
    (cir_comp   (cir1 circuito?)
                (cir2 (list-of circuito?))
                (in (list-of symbol?))
                (out (list-of symbol?))))

(define-datatype chip chip?
    (prim_chip
        (cp chip_prim?))
    (comp_chip
        (c1 (list-of symbol?))
        (c2 (list-of symbol?))
        (cir circuito?)))

(define-datatype chip_prim chip_prim?
    (prim_or)
    (prim_and)
    (prim_nor)
    (prim_nand)
    (prim_not)
    (prim_xor)
    (prim_xnor))

(define cir1
  (cir_comp 
    (cir_simple '(A B) '(S1)
      (prim_chip (prim_xor)))
    (list 
      (cir_simple '(S1 C) '(OUT1)
        (prim_chip (prim_and)))
      (cir_simple '(S1 D) '(OUT2)
        (prim_chip (prim_or)))
    )
    '(A B C D)
    '(OUT1 OUT2)
  ))


(define cir2
  (cir_comp 
    (cir_simple '(A B) '(X)
      (prim_chip (prim_and)))
    (list 
      (cir_simple '(C D) '(Y)
        (prim_chip (prim_or)))
      (cir_simple '(X Y) '(OUT1)
        (prim_chip (prim_nand)))
    )
    '(A B C D)
    '(OUT1)
  ))

(define cir3
  (cir_comp 
    (cir_simple '(A B) '(S1)
      (prim_chip (prim_xor)))
    (list 
      (cir_simple '(S1 C) '(OUT1)
        (prim_chip (prim_and)))
      (cir_simple '(C D) '(OUT2)
        (prim_chip (prim_or)))
    )
    '(A B C D)
    '(OUT1 OUT2)
  ))

  (define cir_comp1
  (cir_comp 
    (cir_simple '(A B C) '(S1)
      (prim_chip (prim_nor)))
    (list 
      (cir_simple '(S1 D) '(OUT1)
        (prim_chip (prim_xnor)))
      (cir_simple '(C D) '(S2)
        (prim_chip (prim_and)))
      (cir_simple '(S2 S1) '(OUT2)
        (prim_chip (prim_or)))
    )
    '(A B C D)
    '(OUT1 OUT2)
  ))

(define cir_comp2
  (cir_comp 
    (cir_simple '(A B C) '(S1)
      (prim_chip (prim_and)))
    (list 
      (cir_simple '(S1 D) '(OUT1)
        (prim_chip (prim_xor)))
      (cir_simple '(C D) '(S2)
        (prim_chip (prim_not)))
      (cir_simple '(S2 E) '(OUT2)
        (prim_chip (prim_or)))
    )
    '(A B C D E)
    '(OUT1 OUT2)
  ))
