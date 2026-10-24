#lang eopl

;******************************************************************************************
;; TALLER 3
;;
;; Integrantes:

;; Puntos X: FERNANDEZ GUZMAN SANTIAGO - 202610133
;; Puntos (1 al 4): MORALES YUGUE IVAN CAMILO - 202610135
;; Puntos X: COBO MENDOZA CRISTIAN CAMILO - 202323571
;; Puntos X: PEREZ UMBARILA JHAN ALEJANDRO - 202510030
;; Repositorio GitHub: https://github.com/FLP-GRUPO8/Taller3
;******************************************************************************************

;******************************************************************************************
;;;;; Interpretador con notación infija
;;
;; Valores denotados:  Texto + Número + Booleano + ProcVal
;; Valores expresados: Texto + Número + Booleano + ProcVal
;;
;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>      ::= <expresion>
;;                      <un-programa (exp)>
;;  <expresion>     ::= <numero>
;;                      <numero-lit (num)>
;;                  ::= "\"" <texto> "\""
;;                      <texto-lit (txt)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= (<expresion> <primitiva-binaria> <expresion>)
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;                  ::= <primitiva-unaria> (<expresion>)
;;                      <primapp-un-exp (prim-unaria exp)>
;;                  ::= Si <expresion> "{" <expresion> "}" "sino" "{" <expresion> "}"
;;                      <condicional-exp (test-exp true-exp false-exp)>
;;                  ::= declarar ({<identificador> = <expresion> ;}*) { <expresion> }
;;                      <variableLocal-exp (ids exps cuerpo)>
;;                  ::= procedimiento ({<identificador>}*(,)) { <expresion> }
;;                      <procedimiento-ex (ids cuerpo)>
;;                  ::= evaluar <expresion> ({<expresion>}*(,)) finEval
;;                      <app-exp (exp exps)>
;;                  ::= declararRec ({<identificador> ({<identificador>}*(,)) = <expresion> ;}*) { <expresion> }
;;                      <declararRec-exp (proc-names idss bodies cuerpo)>
;;  <primitiva-binaria> ::= + | ~ | / | * | concat | > | < | >= | <= | != | ==
;;  <primitiva-unaria>  ::= longitud | add1 | sub1 | neg
;******************************************************************************************
;
;Funciones Auxiliares

;list-find-position: symbol <list-of symbol> -> número | #f
;Posición de un símbolo en una lista de símbolos
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;list-index: predicado lista -> número | #f
;Posición del primer elemento que cumple el predicado
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

;bool->num: booleando -> numero
;convierte un los booleanos #t y #f a 0 y 1 respectivamente
(define bool->num
  (lambda (b)
    (if b 1 0)))

;values-equal?: valor valor -> boolean
;Compara dos valores: con = si ambos son números y con equal? en los demás casos
(define values-equal?
  (lambda (v1 v2)
    (if (and (number? v1) (number? v2))
        (= v1 v2)
        (equal? v1 v2))))

;******************************************************************************************


; Especificación léxica

(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (identificador
     ("@" letter (arbno (or letter digit "_"))) symbol)
    (texto
     (letter (arbno (or letter digit "_"))) string)
    (numero
     (digit (arbno digit)) number)
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number)
    (numero
     ("-" digit (arbno digit)) number)
    (numero
     ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;******************************************************************************************
; Especificación sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)

    (expresion (numero) numero-lit)
    (expresion ("\"" texto "\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("Si" expresion "{" expresion "}" "sino" "{" expresion "}")
               condicional-exp)
    (expresion ("declarar" "(" (arbno identificador "=" expresion ";") ")" "{" expresion "}")
               variableLocal-exp)
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "{" expresion "}")
               procedimiento-ex)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")
               app-exp)
    (expresion ("declararRec" "(" (arbno identificador "(" (separated-list identificador ",") ")"
                                         "=" expresion ";") ")" "{" expresion "}")
               declararRec-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)))

;******************************************************************************************
; Tipos de datos para la sintaxis abstracta de la gramática
; Construidos con SLLGEN (programa, expresion, primitiva-binaria, primitiva-unaria):

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;show-the-datatypes: -> lista
;Muestra las definiciones de datatypes que genera SLLGEN
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El Front End (Análisis léxico (scanner) y sintáctico (parser) integrados)
;scan&parse: string -> <programa>
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador léxico (Scanner)
;just-scan: string -> lista de tokens
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (Front End + Evaluación + señal para lectura)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program pgm))
    (sllgen:make-stream-parser
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> valor
;Evalúa un programa en el ambiente inicial
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                   (evaluar-expresion body (init-env))))))

;init-env: -> <ambiente>
;Ambiente con (@a @b @c @d @e) ligados a (1 2 3 "hola" "FLP")   [corresponde al punto 2]
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;evaluar-expresion: <expresion> <ambiente> -> valor
;Evalúa la expresión en el ambiente dado
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable id env))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let ((val1 (eval-rand exp1 env))
                             (val2 (eval-rand exp2 env)))
                            (apply-primitive-bin prim-binaria val1 val2)))
      (primapp-un-exp (prim-unaria exp)
                      (let ((val (eval-rand exp env)))
                          (apply-primitive-un prim-unaria val)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (evaluar-expresion test-exp env))
                           (evaluar-expresion true-exp env)
                           (evaluar-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                            (let ((args  (eval-rands exps env)))
                              (evaluar-expresion cuerpo (extend-env ids args env))))
      (procedimiento-ex (ids cuerpo)
                        (cerradura ids cuerpo env))
      (app-exp (exp exps)
               (let ((proc (evaluar-expresion exp env))
                     (args (eval-rands exps env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     "Error, no es un procedimiento")))
      (declararRec-exp (proc-names idss bodies cuerpo)
                       (let ((cerraduras
                              (map (lambda (ids body)
                                     (cerradura ids
                                                (declararRec-exp proc-names idss bodies body)
                                                env))
                                   idss bodies)))
                         (evaluar-expresion cuerpo (extend-env proc-names cerraduras env)))))))

;eval-rands: <list-of expresion> <ambiente> -> <list-of valor>
;Evalúa cada expresión de una lista en el ambiente dado
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;eval-rand: <expresion> <ambiente> -> valor
;Evalúa un operando individual
(define eval-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))

;apply-primitive-bin: <primitiva-binaria> valor valor -> valor
;Aplica una primitiva binaria a dos valores ya evaluados   [corresponden al punto 1 y 3]
(define apply-primitive-bin
  (lambda (prim val1 val2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ val1 val2))
      (primitiva-resta () (- val1 val2))
      (primitiva-div () (/ val1 val2))
      (primitiva-multi () (* val1 val2))
      (primitiva-concat () (string-append val1 val2))
      (primitiva-mayor () (bool->num (> val1 val2)))
      (primitiva-menor () (bool->num (< val1 val2)))
      (primitiva-mayor-igual () (bool->num (>= val1 val2)))
      (primitiva-menor-igual () (bool->num (<= val1 val2)))
      (primitiva-diferente () (bool->num (not (values-equal? val1 val2))))
      (primitiva-comparador-igual () (bool->num (values-equal? val1 val2))))))

;valor-verdad?: valor -> boolean
;En una expresión numérica 0 es falso, cualquier otro caso es verdadero   [corresponde al punto 3]
(define valor-verdad?
  (lambda (x)
    (not (and (number? x) (zero? x)))))

;apply-primitive-un: <primitiva-unaria> valor -> valor
;Aplica una primitiva unaria a un valor ya evaluado
(define apply-primitive-un
  (lambda (prim val)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length val))
      (primitiva-add1 () (+ val 1))
      (primitiva-sub1 () (- val 1))
      (primitiva-negacion-booleana () (if (valor-verdad? val)
                                          0 1)))))


;*******************************************************************************************
;Procedimientos   [corresponde al punto 6]

;Cerradura (ProcVal): lista de ids, cuerpo y ambiente donde fue declarado
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)))

;apply-procedure: <procVal> <list-of valor> -> valor
;Evalúa el cuerpo de un procedimiento en el ambiente extendido con sus argumentos   [corresponde al punto 7]
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (lista-ID exp amb)
                 (evaluar-expresion exp (extend-env lista-ID args amb))))))
;*******************************************************************************************
;Ambientes

;Definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (vacio)
  (extendido (syms (list-of symbol?))
             (vals (list-of scheme-value?))
             (env ambiente?))
  )

;scheme-value?: cualquier-cosa -> boolean
;Predicado para los valores del lenguaje
(define scheme-value? (lambda (v) #t))

;empty-env: -> <ambiente>
;Crea un ambiente vacío
(define empty-env
  (lambda ()
    (vacio)))

;extend-env: <list-of symbol> <list-of valor> <ambiente> -> <ambiente>
;Crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extendido syms vals env)))

;buscar-variable: symbol <ambiente> -> valor
;Retorna el valor de la variable si está en el ambiente, en caso contrario
;retorna "Error, la variable no existe"   [corresponde al punto 2]
(define buscar-variable
  (lambda (id env)
    (cases ambiente env
      (vacio () "Error, la variable no existe")
      (extendido (syms vals old-env)
                 (let ((pos (list-find-position id syms)))
                   (if (number? pos)
                       (list-ref vals pos)
                       (buscar-variable id old-env)
                 ))))))


;******************************************************************************************
;Pruebas

;Pendientes
