; Programming Project Part 1
; A simple interpreter for a basic language
; J. Plasmeier | jgp45@case.edu
; 02/13/2017

(load "simpleParser.scm")
(load "lex.scm") ; parser dependency 
(require racket/trace)

; interpreter TODO
; nested assignment (assignment returns value)
(define interpret-file
  (lambda (file)
    (interpret (parser file) (lambda (x) x))))

(define interpret
  (lambda (pt state-cont)
    (begin0  
    (cond
      ((null? pt) state-cont)
      ((eq? 'return (car (car pt))) (eval-return (cadr (car pt)) (state-cont e-s)))
      ((ass? (car pt)) (interpret (cdr pt) (lambda (v) (eval-ass (car pt) (state-cont v)))))
      ((decl? (car pt)) (interpret (cdr pt) (lambda (v) (eval-decl (car pt) (state-cont v)))))
      ((eq? 'begin (car (car pt))) (interpret (cdr pt) (begin-scope (cdr (car pt)) state-cont)))
      ((eq? (if-sym (car pt)) 'if) (interpret (cdr pt) (lambda (v) (eval-if (car pt) (push-frame (state-cont v)) (lambda (x) (push-frame (state-cont x)))))))
      ((eq? (while-sym (car pt)) 'while) (interpret (cdr pt) (lambda (v) (eval-while (car pt) (push-frame (state-cont v)) (lambda (a) (push-frame (state-cont a)))))))
      (else state-cont))
    (display "\n Interpret State: ")
    (print (state-cont e-s))
    )))

;(interpret (cdr (car pt)) (lambda (x) (push-frame (state-cont x))))
(define begin-scope
  (lambda (pt state-cont)
    (interpret pt (lambda (x) (push-frame (state-cont x))))))

(define interpretable?
 (lambda (pt state)
   (cond
     ((null? pt) #t)
     ((or (decl? (car pt) state)
          (ass? (car pt) state)
          (if? (car pt) state)
          (while? (car pt) state)
          (return? (car pt) state)))
     (else #f))))

; Helper Functions

; e-s - empty state. useful for calling state-cont on
(define e-s '((()())))

; listOfOne? - returns TRUE if l is a list containing one atom
(define listOfOne?
  (lambda (l)
    (cond
      ((null? l) #f)
      ((and (or (atom? (car l)) (list? (car l))) (null? (cdr l))) #t)
      (else #f))))

; atom? - returns TRUE when a is an atom
(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

; member* - returns TRUE if atom a is found with l or its sublists
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))
            ))

; isUnary - determines if expr is a unary expr or not.
; (- 5) is unary
; (- 5 9) is not
(define isUnary?
  (lambda (expr)
    (and (not (null? (cdr expr))) (null? (cddr expr)))))

; hasThreeTerms? - determines if expr has 3 terms (if w/o else)
(define hasThreeTerms?
  (lambda (expr)
    (and (not (null? (cdr expr))) (not (null? (cddr expr))) (null? (cdddr expr)))))

; TODO - investigate deprecation 
;        (why not use bool?)
(define valid-expr-bool?
  (lambda (expr state)
    (or
      (or (isVar? (operand1 expr) state)
          (number? (operand1 expr))
          (value? (operand1 expr) state)
        (bool? (operand1 expr) state))
      (or (isVar? (operand2 expr) state)
          (number? (operand1 expr))
          (value? (operand1 expr))
        (bool? (operand2 expr) state)))))

; expr-type?
; functions which determine the type of an expression

; return? - determines if the expression is a return
(define return?
  (lambda (expr state)
    (and (eq? (car expr) 'return) (or (bool? (cadr expr) state) (value? (cadr expr) state))))) 

; decl? - is the expression a declaration?
;         var x;
;         var x = 5;
;         var x = y; (y is declared, assigned)
(define decl?
  (lambda (expr)
    (and (eq? (var-decl expr) 'var)
         (name? (var-name expr))
         )))

; ass? - checks if the expr is a valid assignment statement
;        if the ass-var of the expr is not decalred, throw an error
(define ass?
  (lambda (expr)  
      (eq? (ass-op expr) '=)
           )) ; this is bool or value 
      
; name? - is the expression a name?
        ; a name is a single atom
(define name?
  (lambda (expr)
    (or (atom? expr) (and (not (or (number? expr) (number? (car expr)))) (or (listOfOne? expr) (eq? (cdr expr) (cadr expr)))))))

; value? - is the expr able to be evaluated by value? 
(define value?
  (lambda (aexpr state)
    (cond 
      ((isAssigned? aexpr state) #t)
      ((atom? aexpr) (number? aexpr))
      ((and (name? aexpr) (isVar? aexpr state) (not (isAssigned? aexpr state))) (error "Error: Attempted to derefernce uninitialized variable")) 
      ((ass? aexpr) #t)
      ((eq? (operator aexpr) '+)
       (and (value? (operand1 aexpr) state)
            (value? (operand2 aexpr) state)))
      ((eq? (operator aexpr) '-)
       (or
        (value? (operand1 aexpr) state)
        (and (value? (operand1 aexpr) state)
            (value? (operand2 aexpr) state))))
      ((eq? (operator aexpr) '*)
       (and (value? (operand1 aexpr) state)
            (value? (operand2 aexpr) state)))
      ((eq? (operator aexpr) '/)
       (and (value? (operand1 aexpr) state)
            (value? (operand2 aexpr) state)))
      ((eq? (operator aexpr) '%)
       (and (value? (operand1 aexpr) state)
            (value? (operand2 aexpr) state)))
      (else #f))))

; bool? - is the expr able to be evaluated by eval-bool?
(define bool?
  (lambda (expr state)
    (cond
      ((atom? expr) (or (eq? expr 'TRUE) (eq? expr 'true) (eq? expr 'FALSE) (eq? expr 'false)) ) ; if it's an atom, it must be 'TRUE or 'FALSE or a variable 0_0
      ((eq? (operator expr) '!)
       (or
        (isVar? (operand1 expr) state)
        (bool? (operand1 expr) state)))
      ((eq? (operator expr) '||) (valid-expr-bool? expr state)) ; (valid-expr (op expr state)
      ((eq? (operator expr) '&&) (valid-expr-bool? expr state))
      ((eq? (operator expr) '>) (valid-expr-bool? expr state))
      ((eq? (operator expr) '<) (valid-expr-bool? expr state))
      ((eq? (operator expr) '<=) (valid-expr-bool? expr state))
      ((eq? (operator expr) '>=) (valid-expr-bool? expr state))
      ((eq? (operator expr) '!=)
       (or (and (value? (operand1 expr) state)
                (value? (operand2 expr) state))
           (and (bool? (operand1 expr) state)
                (bool? (operand2 expr) state))))
      ((eq? (operator expr) '==)
       (or (and (value? (operand1 expr) state)
                (value? (operand2 expr) state))
           (and (bool? (operand1 expr) state)
                (bool? (operand2 expr) state))))
      (else #f))))

; eval and state helpers

; isVar? - is this a variable? check current scope first, then recurse
(define isVar?
  (lambda (expr state)
    (cond
      ((null? state) #f)
      ((member* expr (state-vars state)) #t)
      (else (isVar? expr (cdr state))))))

; isAssigned? - is the variable assigned to something?
(define isAssigned?
  (lambda (expr state)
    (cond
      ((null? state) #f) ; searched all frames and came up empty
      ((and (isVar? expr state) (not (eq? (deref expr state) '()))) #t) ; 
      (else (isAssigned? expr (cdr state))))))

; deref - given a variable, find its value in the state
(define deref
  (lambda (var state)
    (cond 
      ((not (isVar? var state)) (error "Null Reference Error"))
      ((null? (state-vars state)) (deref var (cdr state))) ; base case of current stack frame
      ((eq? var (car (state-vars state))) (car (state-values state))) ; found our variable, return it
      (else (deref var (cons (cons (cdr (state-vars state)) (cons (cdr (state-values state)) '())) (cdr state))))))) ; recurse on the state

; push-frame - push a new frame onto the stack
;              eventually this will be CPS
(define push-frame
  (lambda (state)
    (cons '(()()) state)))   
    
; eval-type functions
; evaluates an expression of a certain type

; eval-return - evaluates a return expression
(define eval-return
  (lambda (expr state)
    (begin (print state) (cond
      ((isVar? expr state) (deref expr state))
      ((value? expr state) (eval-value expr state))
      ((bool? expr state) (if (eval-bool expr state) "TRUE" "FALSE"))
      ))))

; eval-decl - evaluate variable declaration
;(if (null? (var-tail expr))
(define eval-decl
  (lambda (expr state)
    (begin0 
      (if (null? (var-tail expr))
        (cons (cons (cons (var-name expr) (state-vars state)) (cons (cons '() (state-values state)) '())) (cdr state)) ; no value, make it the empty list
        (cons (cons (cons (var-name expr) (state-vars state)) (cons (cons (eval-bool-or-val (var-val expr) state) (state-values state)) '())) (cdr state))
        )
     ; (display "\n decl state: ")
      ;(print state)
      )))

; eval-ass - evaluate an assignment
(define eval-ass
  (lambda (expr state)
    (begin (display state) (cond
      ((null? state) '())                      
      ((isVar? (ass-val expr) state) (eval-ass (list (operator expr) (ass-var expr) (deref (ass-val expr) state)) state))
      ((not (isVar? (ass-var expr) state))(error "Error: Attempted to assign to undeclared variable."))
      ((null? (state-vars state)) (cons (car state) (eval-ass expr (cdr state)))) ; base case of current state frame
      ((eq? (ass-var expr) (car (state-vars state))) ; found our variable, return the modified state
       (cons (cons (state-vars state) 
             (cons (cons (eval-bool-or-val (ass-val expr) state) 
                         (cdr (state-values state)))
                   '()))
             (cdr state)))
      (else 
       (cons (cons (cons
                    (car (state-vars state))
                    (state-vars (eval-ass expr 
                                          (cons (cons (cdr (state-vars state)) (cons (cdr (state-values state)) '()))
                                                (eval-ass expr (cdr state))))))
                   (cons
                    (cons
                     (car (state-values state))
                     (state-values (eval-ass expr 
                                             (cons (cons (cdr (state-vars state)) (cons (cdr (state-values state)) '()))
                                                   (eval-ass expr (cdr state)))))) '()))
             (cdr (eval-ass expr 
                            (cons (cons (cdr (state-vars state)) (cons (cdr (state-values state)) '()))
                                  (eval-ass expr (cdr state))))) ))))))  ; the rest of this frame

; eval-if - evaluate an if statement
(define eval-if
  (lambda (expr state state-cont)
    (if
      (eval-bool (if-cond expr) state) ; condition is true
      ((interpret (cdr (caddr expr)) state-cont) e-s)
      (if (hasThreeTerms? expr) ; condition is false - else 
          state ; no else body - return state
          ((interpret (cons (if-else expr) '()) state-cont) e-s)) ))) ; interpret else body
  
; eval-while - evaluate a while loop
;              when this is called, a frame has already been pushed
(define eval-while
  (lambda (expr state state-cont)
    (if 
     (eval-bool (while-cond expr) state)
     (eval-while expr ((interpret (while-body expr) state-cont) e-s) (interpret (while-body expr) state-cont))
     (state-cont e-s))))

; eval-bool-or-val - evaluate an expr of bool or value (numeric) type
(define eval-bool-or-val
  (lambda (expr state)
    (cond
      ((bool? expr state) (if (eval-bool expr state) "TRUE" "FALSE"))
      ((value? expr state) (eval-value expr state))
      ((not (isVar? expr state)) (error "Error: Attempted to dereference undeclared value"))
      (else (error "Neither bool nor value")))))

; get the value of a boolean expression
; can we use #t and #f internally, but return as a string..?
(define eval-bool
  (lambda (expr state)
    (cond
      ((number? expr) (not (zero? expr))) ; any non-zero number is considered TRUE
      ((or (eq? expr 'TRUE) (eq? expr 'true)))
      ((or (eq? expr 'FALSE) (eq? expr 'false)) #f)
      ((atom? expr) #t) ; hmmm, what should general atoms evaluate to? probably tHEIR VALUE IN the STAtE.
      ((eq? (operator expr) '>) (> (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '<) (< (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '>=) (>= (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '<=) (<= (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '==) (eq? (if (value? (operand1 expr) state)
                                          (eval-value (operand1 expr) state) 
                                          (eval-bool (operand1 expr) state)); these are tricky - could be bool OR value
                                      (if (value? (operand2 expr) state) 
                                          (eval-value (operand2 expr) state) 
                                          (eval-bool (operand2 expr) state))))
      ((eq? (operator expr) '!=)  (not (eq? (if (value? (operand1 expr) state)
                                               (eval-value (operand1 expr) state) 
                                               (eval-bool (operand1 expr) state))
                                           (if (value? (operand2 expr) state) 
                                               (eval-value (operand2 expr) state) 
                                               (eval-bool (operand2 expr) state)))))
      ((eq? (operator expr) '||) (or (eval-bool (operand1 expr) state) (eval-bool (operand2 expr) state))) 
      ((eq? (operator expr) '&&) (and (eval-bool (operand1 expr) state) (eval-bool (operand2 expr) state))) 
      ((eq? (operator expr) '!) (not (eval-bool (operand1 expr) state)))
      )))

; get the value of an arithmetic expression       
(define eval-value
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ((isVar? expr state) (deref expr state))
      ((ass? expr) (ass-val expr)) ; wat
      ((eq? (operator expr) '+) (+ (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '*) (* (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((and (isUnary? expr) 
            (eq? (operator expr) '-))
            (- (eval-value (operand1 expr) state)))
      ((eq? (operator expr) '-)
            (- (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '/) (floor (/ (eval-value (operand1 expr) state) (eval-value (operand2 expr) state))))
      ((eq? (operator expr) '%) (modulo (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      (else (error "Unknwn Operator: " (operator expr))))))

; abstraction of state operators
(define state-vars (lambda (s) (car (car s))))
(define state-values (lambda (s) (cadr (car s))))

; abstraction of var declaration
(define var-decl car) ; var
(define var-name cadr) ; x
(define var-val caddr) ; the value after x
(define var-tail cddr) ; anything after x - can check nullity 


; abstraction of if operators
(define if-sym car)
(define if-cond cadr)
(define if-body caddr)
(define if-else cadddr)

; abstraction of while operators
(define while-sym car)
(define while-cond cadr)
(define while-body (lambda (e) (cdr (car (cddr e)))))

; abstraction of assignment
(define ass-op car)
(define ass-var cadr)
(define ass-val caddr)

; define prefix notation
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
