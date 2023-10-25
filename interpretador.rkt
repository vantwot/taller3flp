#lang eopl
;******************************************************************************************
;;;;; Integrantes:
;;;;; SANTIAGO DUQUE CHACÓN-2180099
;;;;; DEIBY A. RODRIGUEZ RODALLEGA-1842917
;;;;; VALENTINA SALAMANCA RODRIGUEZ-1842427
;******************************************************************************************
;;;;; URL del repositorio en github: https://github.com/vantwot/taller3flp.git
;******************************************************************************************

;******************************************************************************************
;                                SOLUCIÓN
;******************************************************************************************

;******************************************************************************************
;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expresion>
;;                      <un-programa (exp)>
;;  <expresion>     ::= <numero>
;;                      <numero-lit (num)>
;;                  ::= <"\"" <texto> "\"">
;;                      <texto-lit (txt)>
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= <(<expresion> <primitiva-binaria> <expresion>)>
;;                      <primapp-bin-exp (exp1 prim-binaria exp2)>
;;                  ::= <primitiva-unaria> (<expresion>)>
;;                      <primapp-un-exp (prim-unaria exp)>
;; <primitiva-binaria> :=  + (primitiva-suma)
;;                     :=  ~ (primitiva-resta)
;;                     :=  / (primitiva-div)
;;                     :=  * (primitiva-multi)
;;                     :=  concat (primitiva-concat)
;;<primitiva-unaria>   :=  longitud (primitiva-longitud)
;;                     :=  add1 (primitiva-add1)
;;                     :=  sub1 (primitiva-sub1)
;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("#" (arbno (not #\newline))) skip)
  (texto
   ((or letter  ":" "!" "$" "_" "-" "|" "%" "&" "°" "<" ">" "^" "[" "]")
    (arbno (or letter digit ":" "!" "$" "_" "-" "|" "%" "&" "°" "<" ">" "^" "[" "]"))) string)
  (identificador
   ("@" letter (arbno (or letter digit))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  )
  )


;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (number) numero-lit)
    (expresion (identificador) var-exp)    
    (expresion
     ("("  expresion primitiva-binaria expresion ")")
     primapp-bin-exp)
    (expresion
     (primitiva-unaria "("expresion ")")
     primapp-un-exp)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)


    ;;;;;;;;;; CONDICIONALES ;;;;;;;;;;
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI")
                condicional-exp)

    ;;;;;;;;;; DECLARAR VARIABLES ;;;;;;;;;;
    (expresion ("declarar" "("(separated-list identificador "=" expresion ";" ) ")" "{" expresion "}")
                variableLocal-exp)

    ;;;;;;;;;; CREAR PROCEDIMIENTOS ;;;;;;;;;;
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc")
                procedimiento-exp)
    ;;;;;;;;;; EVALUAR PROCEDIMIENTOS ;;;;;;;;;;
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval")
                app-exp)
    
    ;;;;;;;;;; RECURSIVOS ;;;;;;;;;;
    (expresion ("funcionRec" (arbno identificador "(" (separated-list identificador ";") ")" "=" expresion)
                             "haga" expresion "finRec")
               recursivo-exp)

    (expresion ("\"" texto "\"") texto-lit)))
    ;;;;;;    

;Tipos de datos para la sintaxis abstracta de la gramática construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))


;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (eval-expression body (init-env))))))

;*******************************************************************************************

; Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e @pi)
     '(1 2 3 "hola" "FLP" 3.14159265)
     (empty-env))))

;*******************************************************************************************

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expresion exp

      (texto-lit (txt) txt)
      
      (numero-lit (datum) datum)
      
      (var-exp (id) (apply-env env id))
      
      (primapp-bin-exp (exp1 prim-binaria exp2)
                   (let ((args (eval-rands (list exp1 exp2) env)))
                     (apply-primitiva-binaria prim-binaria args)))
      
      (primapp-un-exp (prim-unaria exp)
                     (apply-primitiva-unaria prim-unaria (eval-expression exp env)))
      
      (condicional-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      
      (variableLocal-exp (ids rands cuerpo)
               (let ((args (eval-rands rands env)))
                 (eval-expression cuerpo
                                  (extend-env ids args env))))
      
      (procedimiento-exp (ids cuerpo)
                (cerradura ids cuerpo env))
      
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      
      (recursivo-exp (proc-names idss cuerpos letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss cuerpos env))))))



; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitiva: <primitiva> <list-of-expression> -> numero
(define apply-primitiva-binaria
  (lambda (prim args)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ (car args) (cadr args)))
      (primitiva-resta () (- (car args) (cadr args)))
      (primitiva-multi () (* (car args) (cadr args)))
      (primitiva-div () (/ (car args) (cadr args)))
      (primitiva-concat () (string-append (car args) (cadr args))))))

(define apply-primitiva-unaria
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length args))
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1)))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (cerradura
   (lista-ID (list-of symbol?))
   (cuerpo expresion?)
   (env environment?)
   )
  )

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (lista-ID cuerpo env)
               (eval-expression cuerpo (extend-env lista-ID args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))


;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

; *****************************************************************************************************************
;                                          PUNTOS CALIFICABLES
; *****************************************************************************************************************

;; a) Escriba un programa en su lenguaje de programación que contenga un procedimiento areaCirculo que
;; permita calcular el area de un circulo dado un radio (A=PI*r*r). Debe incluir valores flotantes en
;; su lenguaje de programación. Deberá invocarlo utilizando una variable @radio como parámetro:


; Ejemplo con radio = 2.5    // A = 19.6349540625 (aprox)
;  declarar (
;          @radio=2.5;
;          @areaCirculo = procedimiento(@r) haga ((@pi * @r) * @r) finProc
;          ) {
;             evaluar @areaCirculo (@radio) finEval
;                     }


; Ejemplo con radio = 3    // A = 28.274333850000005 (aprox)
;  declarar (
;          @radio=3;
;          @areaCirculo = procedimiento(@r) haga ((@pi * @r) * @r) finProc
;          ) {
;             evaluar @areaCirculo (@radio) finEval
;                     }


; Ejemplo con radio = 10    // A = 314.159265 (aprox)
;  declarar (
;          @radio=3;
;          @areaCirculo = procedimiento(@r) haga ((@pi * @r) * @r) finProc
;          ) {
;             evaluar @areaCirculo (@radio) finEval
;                     }

;-------------------------------------------------------------------------------------------------------

; b) 5pts. Escriba un programa en su lenguaje de programación que contenga un
; procedimiento que permita calcular el factorial de un número n.
; Como la gramática para funciones recursivas debe ser propuesta por el grupo,
; incluya dos ejemplos de uso para el factorial de 5 y el factorial de 10.


; Ejemplo con 5    // 5! = 120
;
;   funcionRec
;          @factorial(@n) = 
;             Si @n 
;                entonces (@n * evaluar @factorial (sub1(@n)) finEval)
;                sino 1 finSI
;          haga
;             evaluar @factorial (5) finEval finRec


; Ejemplo con 10    // 10! = 3628800
;
;   funcionRec
;          @factorial(@n) = 
;             Si @n 
;                entonces (@n * evaluar @factorial (sub1(@n)) finEval)
;                sino 1 finSI
;          haga
;             evaluar @factorial (10) finEval finRec


; Ejemplo con 40    // 40! = 815915283247897734345611269596115894272000000000
;
;   funcionRec
;          @factorial(@n) = 
;             Si @n 
;                entonces (@n * evaluar @factorial (sub1(@n)) finEval)
;                sino 1 finSI
;          haga
;             evaluar @factorial (3) finEval finRec

;-------------------------------------------------------------------------------------------------------
; 
; c) 10pts. Escriba un programa en su lenguaje de programación que contenga un
; procedimiento que permita calcular una suma de forma recursiva.
; Debe hacer uso de las funciones add1 y sub1 (remitase a la clase
; donde se implementó la interfaz con las funciones zero, isZero?, sucessor, predecessor).
; Si no se evidencia el uso de add1 y sub1, el ejercicio no será valido.
; Incluya un llamado a la función recursiva: "evaluar @sumar (4, 5) finEval "


; Ejemplo sumando 4 y 5    // 4 + 5 = 9
;
;  funcionRec
;    @sumar(@x;@y) = Si @x entonces evaluar @sumar (sub1(@x),add1(@y)) finEval sino @y finSI
;    haga
;    evaluar @sumar (4,5) finEval
;  finRec


; Ejemplo sumando 2 y 3    // 2 + 3 = 5
;
;  funcionRec
;    @sumar(@x;@y) = Si @x entonces evaluar @sumar (sub1(@x),add1(@y)) finEval sino @y finSI
;    haga
;    evaluar @sumar (2,3) finEval
;  finRec


; Ejemplo sumando 18 y 21    // 18 + 21 = 39
;
;  funcionRec
;    @sumar(@x;@y) = Si @x entonces evaluar @sumar (sub1(@x),add1(@y)) finEval sino @y finSI
;    haga
;    evaluar @sumar (18,21) finEval
;  finRec

;-------------------------------------------------------------------------------------------------------

; d) 15pts. Escriba un programa en su lenguaje de programación que permita restar y
; multiplicar dos números haciendo uso solamente de las primitivas add1 y sub1.
; Incluya llamados:  "evaluar @restar (10, 3) finEval  ",  "evaluar @multiplicar (10, 3) finEval" 


; Ejemplo restando 10 y 3    // 10 - 3 = 7

;  funcionRec
;    @restar(@x;@y) = Si @y entonces evaluar @restar (sub1(@x),sub1(@y)) finEval sino @x finSI
;    @sumar(@a;@b) = Si @a entonces evaluar @sumar (sub1(@a),add1(@b)) finEval sino @b finSI
;    @multiplicar(@c;@d) = Si @d entonces evaluar @sumar (@c, evaluar @multiplicar (@c,sub1(@d)) finEval) finEval sino 0 finSI
;    haga
;    evaluar @restar (10,3) finEval
;  finRec


; Ejemplo restando 2 y 5    // 2 - 5 = -3

;  funcionRec
;    @restar(@x;@y) = Si @y entonces evaluar @restar (sub1(@x),sub1(@y)) finEval sino @x finSI
;    @sumar(@a;@b) = Si @a entonces evaluar @sumar (sub1(@a),add1(@b)) finEval sino @b finSI
;    @multiplicar(@c;@d) = Si @d entonces evaluar @sumar (@c, evaluar @multiplicar (@c,sub1(@d)) finEval) finEval sino 0 finSI
;    haga
;    evaluar @restar (5,3) finEval
;  finRec


; Ejemplo multiplicando 10 y 3    // 10 * 3 = 30
; funcionRec
;   @restar(@x;@y) = Si @y entonces evaluar @restar (sub1(@x),sub1(@y)) finEval sino @x finSI
;   @sumar(@a;@b) = Si @a entonces evaluar @sumar (sub1(@a),add1(@b)) finEval sino @b finSI
;   @multiplicar(@c;@d) = Si @d entonces evaluar @sumar (@c, evaluar @multiplicar (@c,sub1(@d)) finEval) finEval sino 0 finSI
;   haga
;   evaluar @multiplicar (10,3) finEval
; finRec


; Ejemplo multiplicando 5 y 3    // 5 * 3 = 15

; funcionRec
;   @restar(@x;@y) = Si @y entonces evaluar @restar (sub1(@x),sub1(@y)) finEval sino @x finSI
;   @sumar(@a;@b) = Si @a entonces evaluar @sumar (sub1(@a),add1(@b)) finEval sino @b finSI
;   @multiplicar(@c;@d) = Si @d entonces evaluar @sumar (@c, evaluar @multiplicar (@c,sub1(@d)) finEval) finEval sino 0 finSI
;   haga
;   evaluar @multiplicar (5,3) finEval
; finRec

;-------------------------------------------------------------------------------------------------------

; e) 25pts. Crea una función @integrantes que muestre los nombres de los integrantes del grupo y adicionalmente crea un decorador
; que al invocarlo salude a los integrantes

; declarar (
;    @integrantes = procedimiento () haga "Santiago-Deiby-Valentina" finProc;
;    @saludar = procedimiento (@funcion) haga ("Hola:" concat evaluar @funcion () finEval) finProc
;   ) {
;      declarar (@decorate = procedimiento () haga evaluar @saludar (@integrantes) finEval finProc
;        ) {
;           evaluar @decorate () finEval }}

;-------------------------------------------------------------------------------------------------------

;f) 35pts. Modifique el ejercicio anterior para que el decorador reciba como parámetro otro mensaje que debe ponerse al final de todo
; el string (cualquier implementación sin el concepto de decorador no será evaluada).


; declarar (
;    @integrantes = procedimiento () haga "Santiago-Deiby-Valentina" finProc;
;    @saludar = procedimiento (@funcion) haga ("Hola:" concat evaluar @funcion () finEval) finProc
;  ) {
;      declarar (@decorate = procedimiento (@var) haga (evaluar @saludar (@integrantes) finEval concat @var) finProc){
; evaluar @decorate ("-ProfesoresFLP") finEval }}


; declarar (
;     @integrantes = procedimiento () haga "Santiago-Deiby-Valentina" finProc;
;     @saludar = procedimiento (@funcion) haga ("Hola:" concat evaluar @funcion () finEval) finProc
;   ) {
;      declarar (@decorate = procedimiento (@var) haga (evaluar @saludar (@integrantes) finEval concat @var) finProc){
;          evaluar @decorate (":Esto-es-un-programa-funcional$") finEval }}


