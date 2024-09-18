#lang eopl

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
    (prim_xnor)
)


(define parser-circuito
    (lambda (list)
        (cond 
            [(eq? (car list) 'cir_simple) (cir_simple (cadr list) (caddr list) (parser-chip(cadddr list)) )]
            [(eq? (car list) 'cir_comp) (cir_comp (parser-circuito(cadr list))  (map parser-circuito(caddr list)) (cadddr list) (car (cdddr list)) )]
        )
    )
)

(define parser-chip
    (lambda (list)
        (cond
            [(eq? (car list) 'prim_chip) (prim_chip (parser-chip_prim(cadr list))) ]
            [(eq? (car list) 'comp_chip) (comp_chip (cadr list) (caddr list) (parser-circuito(cadddr list)))]            
        )   
    )
)

(define parser-chip_prim
    (lambda (list)
        (cond
            [(eq? (car list) 'prim_and) (prim_and)]
            [(eq? (car list) 'prim_or) (prim_or)]
            [(eq? (car list) 'prim_not) (prim_not)]
            [(eq? (car list) 'prim_xor) (prim_xor)]
            [(eq? (car list) 'prim_nand) (prim_nand)]
            [(eq? (car list) 'prim_nor) (prim_nor)]
            [(eq? (car list) 'prim_xnor) (prim_xnor)]            
        )
    )
)

;Area del programador

(define chip1
  '(comp_chip
    (INA INB INC IND)
    (OUTA)
    (cir_comp
      (cir_simple (a b) (e)
        (prim_chip (prim_and)))
      ((cir_simple (c d) (f)
         (prim_chip (prim_and)))
       (cir_simple (e f) (g)
         (prim_chip (prim_or))))
      (a b c d)
      (g))))

(define circuito1
  '(cir_comp
    (cir_simple (a b) (e)
      (prim_chip (prim_and)))
    ((cir_simple (c d) (f)
       (prim_chip (prim_and)))
     (cir_simple (e f) (g)
       (prim_chip (prim_or))))
    (a b c d)
    (g)))


