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