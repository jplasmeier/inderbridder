; Programming Project Part 1
; A simple interpreter for a basic language
; J. Plasmeier | jgp45@case.edu
; 02/13/2017

(load "simpleParser.scm")
(load "lex.scm") ; parser dependency 

; interpreter TODO
; nested assignment (assignment returns value)
(define interpret-file
  (lambda (file)
    (interpret (parser file) '(()()))))

; interpret 
(define interpret
  (lambda (pt state)
    (cond
      ((null? pt) state)
      ((return? (car pt) state) (eval-return (cadr (car pt)) state))
      ((decl? (car pt) state) (interpret (cdr pt) (eval-decl (car pt) state)))
      ((ass? (car pt) state) (interpret (cdr pt) (eval-ass (car pt) state)))
      ((if? (car pt) state) (interpret (cdr pt) (eval-if (car pt) state)))
      ((while? (car pt) state) (interpret (cdr pt) (eval-while (car pt) state)))
      (else (error "Error: Interpreter could not evaluate the expression.")))))

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
      (else (or (member* a (car l)) (member* a (cdr l)))))))

; expr-type?
; functions which determine the type of an expression

; isUnary - determines if expr is a unary expr or not.
; (- 5) is unary
; (- 5 9) is not
(define isUnary?
  (lambda (expr)
    (equal? (cons (cadr expr) '()) (cdr expr))))

; return? - determines if the expression is a return
(define return?
  (lambda (expr state)
    (and (eq? (car expr) 'return) (or (bool? (cadr expr) state) (value? (cadr expr) state)))))

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

; decl? - is the expression a declaration?
;         var x;
;         var x = 5;
;         var x = y; (y is declared, assigned)
(define decl?
  (lambda (expr state)
    (and (eq? (var-decl expr) 'var)
         (name? (var-name expr))
         (or (null? (var-tail expr)) ; var-tail is cddr, so is null when expr is like (var x)
             (value? (var-val expr) state) ; var-val is cadr, so if there is a value like (var x 5) it is that value
             (bool? (var-val expr) state)))))

; ass? - checks if the expr is a valid assignment statement
;        if the ass-var of the expr is not decalred, throw an error
(define ass?
  (lambda (expr state)  
      (and (eq? (ass-op expr) '=)
           (or (name? (ass-val expr))
               (value? (ass-val expr) state)
               (bool? (ass-val expr) state))))) ; this is bool or value 
      

; if? - checks if the expr is a valid if statement
(define if?
  (lambda (expr state) 
      (and (eq? (if-sym expr) 'if) (bool? (if-cond expr) state) (interpretable? (cons (if-body expr) '()) state))))

; while? - checks if the expr is a valid while loop statement
(define while?
  (lambda (expr state)
      (and (eq? (while-sym expr) 'while) (bool? (while-cond expr) state) (interpretable? (cons (while-body expr) '()) state))))

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
      ((and (isVar? aexpr state) (not (isAssigned? aexpr state))) (error "Error: Attempted to derefernce uninitialized variable"))
      ((atom? aexpr) (number? aexpr))      
      ((ass? aexpr state) #t)
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


; eval-type functions
; evaluates an expression of a certain type

; eval-return - evaluates a return expression
(define eval-return
  (lambda (expr state)
    (cond
      ((value? expr state) (eval-value expr state))
      ((bool? expr state) (if (eval-bool expr state) "TRUE" "FALSE"))
      ((isVar? expr state)))))


; eval-decl - evaluate variable declaration
;(if (null? (var-tail expr))
(define eval-decl
  (lambda (expr state)
    (if (null? (var-tail expr))
        (cons (cons (var-name expr) (state-vars state)) (cons (cons '() (state-values state)) '())) ; no value, make it the empty list
        (cons (cons (var-name expr) (state-vars state)) (cons (cons (eval-bool-or-val (var-val expr) state) (state-values state)) '()))
        )))
  
(define eval-ass
  (lambda (expr state)
    (cond
      ((null? (state-vars state)) (error "Error assigning variable"))
      ((not (isVar? (ass-var expr) state)) (error "Error: Attempted to assign to undeclared variable")) 
      ((eq? (ass-var expr) (car (state-vars state))) ; found the variable
       (cons (state-vars state) 
             (cons (cons (eval-bool-or-val (ass-val expr) state) 
                         (cdr (state-values state)))
                   '())))
      (else (cons  ; didn't find the variable, recurse while preserving the state
             (state-vars state)
             (cons 
              (cons (car (state-values state))
                   (state-values (eval-ass expr (cons 
                       (cdr (state-vars state))
                       (cons 
                        (cdr (state-values state))
                        '())))))
              '()))))))

(define eval-if
  (lambda (expr state)
    (if
      (has-else? expr) 
      (eval-if-else expr state)
      (if (eval-bool (if-cond expr) state) 
          (interpret (cons (if-body expr) '()) state)
          state)))) 
      
(define has-else?
  (lambda (expr)
    (not (listOfOne? (cdr (cdr expr))))))

(define eval-if-else
  (lambda (expr state)
    (if (eval-bool (if-cond expr) state) (interpret (cons (if-body expr) '()) state) (interpret (cons (if-else expr) '()) state))))
  
; eval-while - evaluate a while loop
(define eval-while
  (lambda (expr state)
    (if 
     (eval-bool (while-cond expr) state)
     (eval-while expr (interpret (cons (while-body expr) '()) state))
     state)))

; is this a variable?
; that is, is it in the state?
(define isVar?
  (lambda (expr state)
    (member* expr (state-vars state))))

(define isAssigned?
  (lambda (expr state)
    (cond
      ((not (isVar? expr state)) #f) ; if the variable isn't declared, it's definitely not assigned
      (else (not (eq? (deref expr state) '()))))))

; deref - given a variable, find its value in the state
(define deref
  (lambda (var state)
    (cond
      ((not (isVar? var state)) (error "Null reference ehuehuehuhe"))
      ((eq? (car (state-vars state)) var) (car (state-values state)))
      (else (deref var (cons (cdr (state-vars state)) (cons (cdr (state-values state)) '())))))))

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
      ((ass? expr state) (ass-val expr))
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
(define state-vars car)
(define state-values cadr)

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
(define while-body caddr)

; abstraction of assignment
(define ass-op car)
(define ass-var cadr)
(define ass-val caddr)

; define prefix notation
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
