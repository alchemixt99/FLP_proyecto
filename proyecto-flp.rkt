#lang eopl
(require racket/string)

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local y procedimientos

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <caracter>
;;                      <cht-def>
;;                  ::= <cadena>
;;                      <str-def>
;;                  ::= <ok>
;;                  
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {identifier = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;
;;  <primitive>     ::= + | - | * | / | % | &
;;
;;  <bool-primitive>::= < | > | <= | >= | is
;;  <bool-oper>     ::= not | and | or


;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ( "(*" (arbno (not #\newline)) "*)") skip)
  (identifier
   (letter (arbno (or letter digit "?" "-" "_" ))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  #|Definición de char|#
  (cht-def
   ( "\'" letter "\'" ) symbol)
  #|Definición de String|#
  (str-def
   ( "\""(or letter whitespace digit) (arbno (or whitespace letter digit)) "\"") string)
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression (cht-def) cht-exp)
    (expression (str-def) str-exp)
    (expression ("ok") ok-exp)
    (expression
     (primitive "(" (separated-list expression ",")")") primapp-exp)
    #|var|#
    (expresion
     ("var" (separated-list identifier "=" expression ",") "in" expression "end") var-exp)
    
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    
    ; características adicionales
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    
    #|Boolean expressions - OBLIQ|#
    (bool-expression
     (bool-primitive "(" (arbno expression)")") app-bool-exp)
    (bool-expression
     (bool-oper "(" (arbno bool-expresion)")") oper-bool-exp)
    (bool-expression ("true") true-exp)
    (bool-expression ("false") false-exp)
    
    #|Boolean Primitives  - OBLIQ|#
    (bool-primitive ("<") menor-bprim)
    (bool-primitive (">") mayor-bprim)
    (bool-primitive (">=") mayor-o-igual-bprim)
    (bool-primitive ("<=") menor-o-igual-bprim)
    (bool-primitive ("is") igual-igual-bprim)
    
    ;;(bool-oper ("not") not-prim)
    
    
    
    #|Primitive - OBLIQ|#
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("%") modl-prim)
    (primitive ("&") andp-prim)
    ))


;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?))
;  (proc-exp
;   (ids (list-of symbol?))
;   (body expression?))
;  (app-exp
;   (proc expression?)
;   (args (list-of expression?))))

;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construidos automáticamente:

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
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env
     '(x y z f)
     (list 4 2 5 (closure '(x y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (add-prim) (list (var-exp 'x) (var-exp 'x)) ) '())))
                      (empty-env)))
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (cht-exp (chr) chr)
      (str-exp (cd) 
                  (let ((end (- (string-length cd) 1))) #|Quitamos el ultimo caracter de la cadena|#
                      (substring cd 1 end)))            #|Quitamos el primer elemento de la cadena|#
      (ok-exp () "ok")
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (var-exp (ids ids-exps body-exp)                       #||#
              (let ((id-values (crear-celdas ids-exps env))) ; se crea una celda por cada ids-exps y se ligan a id-values
                     (eval-expresion body-exp (extend-env ids id-values env))))
      
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      #|Bool primitive case|#
      (b-primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-bool-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

()

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))
      (modl-prim () (modulo (car args) (cadr args)))
      (andp-prim () (string-append (car args) (cadr args)))
      )))


;apply-bool-primitive: <primitiva> <list-of-expression> -> bool
(define apply-bool-primitive
  (lambda (prim args)
    (cases bool-primitive prim
      (menor-bprim () (< (car args) (cadr args))) ;;menor
      (mayor-bprim () (> (car args) (cadr args))) ;;mayor
      (mayor-o-igual-bprim () (>= (car args) (cadr args))) ;;mayor o igual
      (menor-o-igual-bprim () (<= (car args) (cadr args))) ;;menor o igual
      (igual-igual-bprim () (eqv? (car args) (cadr args))) ;;is
      )))

;eval-bool-expression: <bool-exp> <env> -> <bool>
(define eval-bool-expression
  (lambda (test-exp env)
    (cases bool-expresion test-exp
      (true-exp() #t)
      (false-exp() #f)
      (app-bool-exp (oper args)
                  (let ((args (eval-rands args env)))  ; se evaluan los argumentos
                    (aplicar-prim-bool oper args)))     ; y se llama a la funcion auxiliar que evaluara la expresion y retorna su valor de verdad
      (oper-bool (oper args) 
                  (aplicar-bool-oper oper args env)))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (exp)
  (cond ((eqv? exp 'true)#t)
  ((eqv? exp 'false) #f)
  ((= exp 0) #f)
  (else #t))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
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

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))


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

;******************************************************************************************
#| Run interpreter |#
(interpretador)
;Pruebas
#|
let
calc =
object {
arg => 0,
acc => 0,
          enter =>       (* entra un nuevo argumento *)
meth(s, n)
update s.arg := n
end,
add =>         (* la suma *)
meth(s)
update s.acc := send s.equals();
update s.equals := meth(s) +(get s.acc, get s.arg) end
end,
sub =>         (* la resta *)
meth(s)
update s.acc := send s.equals();
update s.equals := meth(s) -(get s.acc, get s.arg) end
end,
equals =>      (* el resultado *)
meth(s)
get s.arg 
end,
reset =>       (* inicializar *)
meth(s)
update s.arg := 0;
update s.acc := 0;
                          update s.equals := meth(s) get s.arg end
end
};
in
  begin
    send calc.reset(); send calc.enter(3); send calc.add();
    send calc.add(); send calc.equals() (* 9 *)
  end
end
|#
