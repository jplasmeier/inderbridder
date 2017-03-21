; Programming Project Part 1
; A simple interpreter for a basic language
; J. Plasmeier | jgp45@case.edu
; 02/13/2017
(load "simpleParser.scm")

; interpreter-file - parsers a file and interprets its code
(define interpret-file
  (lambda (file)
    (interpret (parser file))))

(define interpret
  (lambda (l)
    (call/cc ; creates a break with the current control context: the state at this time, including the call stack
     (lambda (return-here)
       (letrec ((loop (lambda (l state-cont)
                        (loop (cdr l) (evaluate (car l) 
                                                state-cont 
                                                return-here 
                                                (lambda (e) (error "Error: continue called outside of while body"))
                                                (lambda (e) (error "Error: break called outside scoped block"))))
                        )))
                (loop l (lambda (x) x)))
       ))))

(define evaluate
  (lambda (l state-cont return continue break)
    (cond
      ((null? l) e-s)
      ((eq? (car l) 'return) (return (eval-bool-or-val (cadr l) (state-cont e-s))))
      ((eq? (car l) 'begin) (pop-frame-cont (eval-begin (cdr l) 
                                                        (push-frame-cont state-cont) 
                                                        return 
                                                        (lambda (c) (continue (pop-frame-cont c))) 
                                                        (lambda (b) (break (pop-frame-cont b))))))
      ((eq? (car l) 'break) (break state-cont))
      ((eq? (car l) 'if) (eval-if l state-cont return continue break))
      ((eq? (car l) 'while) (call/cc
                             (lambda (break-here)
                               (eval-while l state-cont return continue break-here))))
      ((eq? (car l) 'continue) (continue state-cont))
      ((eq? (car l) 'var) (lambda (v) (eval-decl l (state-cont v))))
      ((eq? (car l) '=) (lambda (v) (eval-ass l (state-cont v))))
      (else (error "err: fell thru")) )))

(define eval-begin
  (lambda (expr state-cont return continue break)
    (if (null? expr)
        state-cont
        (eval-begin (cdr expr) (evaluate (car expr) state-cont return continue break) return continue break))))

(define eval-if
  (lambda (expr state-cont return continue break)
    (if (eval-bool (if-cond expr) (state-cont e-s))
        (evaluate (caddr expr) state-cont return continue break)
        (if (hasThreeTerms? expr)
            state-cont
            (evaluate (if-elsex expr) state-cont return continue break)))))

(define eval-while
  (lambda (expr state-cont return continue break)
    (if (eval-bool (while-cond expr) (state-cont e-s))
        (eval-while expr 
                    (call/cc
                     (lambda (continue-here)
                       (evaluate (caddr expr) state-cont return continue-here break))) 
                    return
                    continue
                    break)
        state-cont)))

(define push-frame-cont
  (lambda (state-cont)
    (lambda (x) (push-frame (state-cont x)))))

(define pop-frame-cont
  (lambda (state-cont)
    (lambda (x) (pop-frame (state-cont x)))))

; Helper Functions

; e-s - empty state. useful for calling state-cont on
(define e-s '((()())))

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

; ass? - checks if the expr is a valid assignment statement
;        if the ass-var of the expr is not decalred, throw an error
(define ass?
  (lambda (expr)  
      (eq? (ass-op expr) '=)))

; value? - is the expr able to be evaluated by value? 
(define value?
  (lambda (aexpr state)
    (cond 
      ((isAssigned? aexpr state) #t)
      ((atom? aexpr) (number? aexpr))
      ((and (isVar? aexpr state) (not (isAssigned? aexpr state))) (error "Error: Attempted to derefernce uninitialized variable")) 
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
      ((atom? expr) (or (eq? expr 'TRUE) (eq? expr 'true) (eq? expr "TRUE") (eq? expr 'FALSE) (eq? expr 'false) (eq? expr "FALSE")) ) ; if it's an atom, it must be 'TRUE or 'FALSE or a variable 0_0
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
(define push-frame
  (lambda (state)
    (cons '(()()) state)))

; pop-frame - pop the first frame off of the stack
(define pop-frame
  (lambda (state)
    (cdr state)))
    
; eval-type functions
; evaluates an expression of a certain type

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
    (cond
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
                                  (eval-ass expr (cdr state))))) )))))  ; the rest of this frame


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
      ((or (eq? expr 'TRUE) (eq? expr 'true) (eq? expr "TRUE")))
      ((or (eq? expr 'FALSE) (eq? expr 'false) (eq? expr "FALSE")) #f)
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
(define if-elsex cadddr)

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
