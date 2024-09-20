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
    (prim_xnor)
)

;Parser
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

; Unparser
(define unparser-circuito
  (lambda (ast)
    (cases circuito ast
      (cir_simple (c1 c2 ch) (list 'cir_simple c1 c2 (unparser-chip ch)))
      (cir_comp (cir1 cir2 in out) (list 'cir_comp (unparser-circuito cir1) (map unparser-circuito cir2) in out)))))

(define unparser-chip
  (lambda (ast)
    (cases chip ast
      (prim_chip (cp) (list 'prim_chip (unparser-chip_prim cp)))
      (comp_chip (c1 c2 cir) (list 'comp_chip c1 c2 (unparser-circuito cir))))))

(define unparser-chip_prim
  (lambda (ast)
    (cases chip_prim ast
      (prim_and () (list 'prim_and))
      (prim_or () (list 'prim_or))
      (prim_not () (list 'prim_not))
      (prim_xor () (list 'prim_xor))
      (prim_nand () (list 'prim_nand))
      (prim_nor () (list 'prim_nor))
      (prim_xnor () (list 'prim_xnor)))))



;Area del programador

;Listas
(define chip1-list
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

(define cir1-list
  '(cir_comp
    (cir_simple (a b) (e)
      (prim_chip (prim_and)))
    ((cir_simple (c d) (f)
       (prim_chip (prim_and)))
     (cir_simple (e f) (g)
       (prim_chip (prim_or))))
    (a b c d)
    (g)))

(define cir2-list
  '(cir_comp
    (cir_simple (m n o p) (e f)
      (comp_chip
        (INA INB INC IND)
        (OUTE OUTF)
        (cir_comp
          (cir_simple (a b) (e)
            (prim_chip (prim_and)))  
          ((cir_simple (c d) (f)
             (prim_chip (prim_and)))) 
          (a b c d)
          (e f))))
    ((cir_simple (e f) (z)
       (comp_chip
         (INE INF)
         (OUTA)
         (cir_simple (e f) (g)
           (prim_chip (prim_or)))))) 
    (m n o p)
    (z)))




; AST
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

(define cir1
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

(define cir2
  (cir_comp
   (cir_simple '(m n o p) '(e f)
    (comp_chip
     '(INA INB INC IND)
     '(OUTE OUTF)
     (cir_comp
      (cir_simple '(a b) '(e)
        (prim_chip (prim_and)))  
      (list
       (cir_simple '(c d) '(f)
         (prim_chip (prim_and))))  
      '(a b c d)
      '(e f))))
   (list
    (cir_simple '(e f) '(z)
     (comp_chip
      '(INE INF)
      '(OUTA)
      (cir_simple '(e f) '(g)
        (prim_chip (prim_or))))))  
   '(m n o p)
   '(z)))

