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
;Ejemlos

;Listas
(define chip2-list
  '(comp_chip
    (INX INY INZ)
    (OUTX)
    (cir_comp
      (cir_simple (x1 x2) (y1)
        (prim_chip (prim_xor)))
      ((cir_simple (x3 x4) (y2)
         (prim_chip (prim_nand)))
       (cir_simple (y1 y2) (y3)
         (prim_chip (prim_and))))
      (x1 x2 x3 x4)
      (y3))))

(define chip3-list
  '(comp_chip
    (IN1 IN2 IN3)
    (OUT1)
    (cir_comp
      (cir_simple (x y) (z)
        (prim_chip (prim_nor)))
      ((cir_simple (p q) (r)
         (prim_chip (prim_xor)))
       (cir_simple (z r) (s)
         (prim_chip (prim_not))))
      (x y p q)
      (s))))      

(define cir3-list
  '(cir_comp
    (cir_simple (p q) (r)
      (prim_chip (prim_or)))
    ((cir_simple (s t) (u)
       (prim_chip (prim_xnor)))
     (cir_simple (r u) (v)
       (prim_chip (prim_not))))
    (p q s t)
    (v)))

(define cir4-list
  '(cir_comp
    (cir_simple (m n o) (p q)
      (comp_chip
        (IN1 IN2 IN3)
        (OUT1 OUT2)
        (cir_comp
          (cir_simple (a b) (c)
            (prim_chip (prim_nor)))
          ((cir_simple (d e) (f)
             (prim_chip (prim_nand))))
          (a b d e)
          (c f))))
    ((cir_simple (p q) (r)
       (comp_chip
         (IN4 IN5)
         (OUT3)
         (cir_simple (p q) (s)
           (prim_chip (prim_xor)))))) 
    (m n o)
    (r)))

(define cir5-list
  '(cir_comp
    (cir_simple (a1 a2) (b1)
      (prim_chip (prim_nand)))
    ((cir_simple (b1 b2) (c1)
       (prim_chip (prim_or)))
     (cir_simple (c1 c2) (d1)
       (prim_chip (prim_and))))
    (a1 a2 b2 c2)
    (d1)))    



; AST
(define chip2
  (comp_chip
    '(INX INY INZ)
    '(OUTX)
    (cir_comp
      (cir_simple '(x1 x2) '(y1)
        (prim_chip (prim_xor)))
      (list
        (cir_simple '(x3 x4) '(y2)
          (prim_chip (prim_nand)))
        (cir_simple '(y1 y2) '(y3)
          (prim_chip (prim_and))))
      '(x1 x2 x3 x4)
      '(y3))))

(define chip3
  (comp_chip
    '(IN1 IN2 IN3)
    '(OUT1)
    (cir_comp
      (cir_simple '(x y) '(z)
        (prim_chip (prim_nor)))
      (list
        (cir_simple '(p q) '(r)
          (prim_chip (prim_xor)))
        (cir_simple '(z r) '(s)
          (prim_chip (prim_not))))
      '(x y p q)
      '(s))))

(define cir3
  (cir_comp
    (cir_simple '(p q) '(r)
      (prim_chip (prim_or)))
    (list
      (cir_simple '(s t) '(u)
        (prim_chip (prim_xnor)))
      (cir_simple '(r u) '(v)
        (prim_chip (prim_not))))
    '(p q s t)
    '(v)))

(define cir4
  (cir_comp
   (cir_simple '(m n o) '(p q)
    (comp_chip
     '(IN1 IN2 IN3)
     '(OUT1 OUT2)
     (cir_comp
      (cir_simple '(a b) '(c)
        (prim_chip (prim_nor)))  
      (list
       (cir_simple '(d e) '(f)
         (prim_chip (prim_nand))))  
      '(a b d e)
      '(c f))))
   (list
    (cir_simple '(p q) '(r)
     (comp_chip
      '(IN4 IN5)
      '(OUT3)
      (cir_simple '(p q) '(s)
        (prim_chip (prim_xor))))))  
   '(m n o)
   '(r)))

(define cir5
  (cir_comp
    (cir_simple '(a1 a2) '(b1)
      (prim_chip (prim_nand)))
    (list
      (cir_simple '(b1 b2) '(c1)
        (prim_chip (prim_or)))
      (cir_simple '(c1 c2) '(d1)
        (prim_chip (prim_and))))
    '(a1 a2 b2 c2)
    '(d1)))

