; Programming Project Part 1
; A simple interpreter for a basic language
; J. Plasmeier | jgp45@case.edu
; 02/13/2017

(load "simpleParser.scm")
(load "lex.scm") ; parser dependency 

(parser "test.jabbascript")
;(parser "test2.jabbascript")

; interpreter TODO
; implement return
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
      ((decl? (car pt)) (interpret (cdr pt) (eval-decl (car pt) state)))
      ((decl-val? (car pt) state) (interpret (cdr pt) (eval-decl-val (car pt) state)))
      ((ass? (car pt) state) (interpret (cdr pt) (eval-ass (car pt) state)))
      ((if? (car pt) state) (interpret (cdr pt) (eval-if (car pt) state)))
      ;((while? (car pt)) (cons (eval-while (car pt)) (interpret (cdr pt) state)))
      ((value? (car pt) state) (cons (eval-value (car pt)) (interpret (cdr pt) state)))
      (else (error "Holy shit what did you do?")))))

(define interpretable?
 (lambda (pt state)
   (cond
     ((null? pt) #t)
     ((or (decl? (car pt))
          (ass? (car pt) state)
          (if? (car pt) state)
          ;(while? (car pt))
          (value? (car pt) state)))
     (else #f))))


; STUFF FROM PREVIOUS LECTURES

; listOfOne? - returns true if l is a list containing one atom
(define listOfOne?
  (lambda (l)
    (cond
      ((null? l) #f)
      ((and (or (atom? (car l)) (list? (car l))) (null? (cdr l))) #t)
      (else #f))))

; atom? - returns true when a is an atom
(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

; member* - returns true if atom a is found with l or its sublists
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

; return? - determines if the expression is a return
(define return?
  (lambda (expr state)
    (and (eq? (car expr) 'return) (or (bool? (cadr expr) state) (value? (cadr expr) state)))))

(define eval-return
  (lambda (expr state)
    (cond
      ((value? expr state) (eval-value expr state))
      ((bool? expr state) (eval-bool expr state))
      ((isVar? expr state)))))

; value? - is the expr able to be evaluated by value? 
(define value?
  (lambda (aexpr state)
    (cond 
      ((isVar? aexpr state) #t)
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

; bool? - is the expr able to be evaluated by eval-bool?
(define bool?
  (lambda (expr state)
    (cond
      ((atom? expr) (or (eq? expr 'true) (eq? expr 'false)) ) ; if it's an atom, it must be 'true or 'false or a variable 0_0
      ((eq? (operator expr) '!)
       (or
        (isVar? (operand1 expr) state)
        (bool? (operand1 expr) state)))
      ((eq? (operator expr) '||) (valid-expr-bool expr state)) ; (valid-expr (op expr state)
      ((eq? (operator expr) '&&) (valid-expr-bool expr state))
      ((eq? (operator expr) '>) (valid-expr-bool expr state))
      ((eq? (operator expr) '<) (valid-expr-bool expr state))
      ((eq? (operator expr) '<=) (valid-expr-bool expr state))
      ((eq? (operator expr) '>=) (valid-expr-bool expr state))
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

(define valid-expr-bool
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
(define decl?
  (lambda (aexpr)
    (cond
      ((eq? (var-decl aexpr) 'var) 
       (and (name? (var-name aexpr)))) 
      (else #f))))

; decl-val? - is the expression a variable declaration with value?
(define decl-val?
  (lambda (expr state)
    (and (atom? (decl-name expr)) (eq? (var-decl expr) 'var) (or (value? (var-val expr) state) (bool? (var-val expr) state)))))

; assign of value of declare muhfuckah
; (eval-decl (var x)  state)
(define eval-decl-val
  (lambda (expr state) 
    (eval-ass (list '= (var-name-abs expr) (var-val expr)) (eval-decl (list 'var (var-name-abs expr)) state))
    )) 

; name? - is the expression a name?
        ; a name is a single atom
(define name?
  (lambda (expr)
    (or (atom? expr) (and (not (or (number? expr) (number? (car expr)))) (or (listOfOne? expr) (eq? (cdr expr) (cadr expr)))))))
    
; eval-decl - evaluate variable declaration
(define eval-decl
  (lambda (expr state)
    (cons (cons (car (var-name expr)) (state-vars state)) (cons (cons '() (state-values state)) '())))) ; declare an unassigned variable as the empty list
                                                                                               ; why the empty list? I'd tell you, but I'd have to kill you.

; ass? - checks if the expr is a valid assignment statement
;        currently if the variable is not already in the state we return false
;        there's probably a different/better way to address this
(define ass?
  (lambda (expr state)
    (cond  
      ((atom? expr) (number? expr)) ; treat a number as an assignment?¿
      ((and (isVar? (ass-var expr) state)  ; the first atom is a declared variable 
           (eq? (ass-op expr) '=)
           (value? (ass-val expr) state)))
      (else #f))))

(define eval-ass
  (lambda (expr state)
    (cond
      ((null? (state-vars state)) (error "error assigning variable"))
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

; if? - checks if the expr is a valid if statement
(define if?
  (lambda (expr state)
    (cond 
      ((and (eq? (if-sym expr) 'if) (bool? (if-cond expr) state) (interpretable? (cons (if-body expr) '()) state))))))

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
  
; is this a variable?
; that is, is it in the state?
(define isVar?
  (lambda (expr state)
      (member* expr (state-vars state))))

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
      ((bool? expr state) (eval-bool expr state))
      ((value? expr state) (eval-value expr state))
      (else (error "Neither bool nor value")))))

; get the value of a boolean expression
; for now we will rely on Scheme's built in #t and #f
; 5 > 2 -> #t
(define eval-bool
  (lambda (expr state)
    (cond
      ((number? expr) (not (zero? expr))) ; any non-zero number is considered true
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
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
      ((eq? (operator expr) '!=) (not (eq? (if (value? (operand1 expr) state)
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
      ((and (eq? (operator expr) '-) 
            (eq? (car (cdr expr)) (operand1 expr)))
            (- (eval-value (operand1 expr) state)))
      ((eq? (operator expr) '-)
            (- (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '/) (/ (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '%) (modulo (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      (else (error "Unknwn Operator: " (operator expr))))))

; abstraction of state operators
(define state-vars car)
(define state-values cadr)

; abstraction of var declaration
(define var-val caddr)
(define decl-name cadr)
(define var-decl car)
(define var-name cdr) ; cdr or cadr?
; actually this is kind of a long story
; it's rather difficult to check for valid names for cadr
; because cadr will always return an atom and we can't check for more atoms so (var x y z) should fail but doesn't 
; what the fuck was i thinking
(define var-name-abs cadr)

; abstraction of if operators
(define if-sym car)
(define if-cond cadr)
(define if-body caddr)
(define if-else cadddr)

; abstraction of assignment
(define ass-op car)
(define ass-var cadr)
(define ass-val caddr)

; define prefix notation
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
