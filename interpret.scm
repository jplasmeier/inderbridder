; Programming Project Part 3
; Justin Plasmeier
(load "functionParser.scm")

; interpreter-file - parsers a file and interprets its code
(define interpret
  (lambda (file)
    (interpreter (parser file))))

(define interpreter
  (lambda (l)
    (call/cc ; creates a break with the current control context: the state at this time, including the call stack
     (lambda (return-here)
       (letrec ((loop (lambda (l state-cont)
                        (loop (cdr l) (evaluate (car l) 
                                                state-cont 
                                                return-here 
                                                (lambda (e) (error "Error: continue called outside of while body"))
                                                (lambda (e) (error "Error: break called outside scoped block"))
                                                (lambda (t e) (error "Error: throw called outside of try block"))))
                        )))
                (loop l (lambda (x) x)))
       ))))

(define evaluate
  (lambda (l state-cont return continue break throw)
    ;(begin (begin (display "\n") (print (state-cont e-s))) 
    (cond
      ((null? l) e-s)
      ((eq? (operator l) 'return) (return (eval-bool-or-val (cadr l) (state-cont e-s))))
      ((eq? (operator l) 'begin) (pop-frame-cont (eval-begin (cdr l) 
                                                        (push-frame-cont state-cont) 
                                                        return 
                                                        (lambda (c) (continue (pop-frame-cont c))) 
                                                        (lambda (b) (break (pop-frame-cont b)))
                                                        (lambda (t e) (throw (pop-frame-cont t) e)))))
      ((eq? (operator l) 'break) (break state-cont))
      ((eq? (operator l) 'throw) (throw state-cont (eval-bool-or-val (operand1 l) (state-cont e-s))))
      ((eq? (operator l) 'if)  (eval-if l 
                                        state-cont 
                                        return 
                                        continue 
                                        break 
                                        throw))
      ((eq? (operator l) 'while) (call/cc
                                  (lambda (break-here)
                                    (eval-while l state-cont return continue break-here throw))))
      ((eq? (operator l) 'continue) (continue state-cont))
      ((eq? (operator l) 'try) (handle-try l state-cont return continue break throw))
      ((eq? (operator l) 'var) (eval-decl l state-cont))
      ((eq? (operator l) 'funcall) (pop-frame-cont (eval-func (cdr l)
                                                              (push-frame-cont state-cont)
                                                              return
                                                              (lambda (c) (continue (pop-frame-cont c))) 
                                                              (lambda (b) (break (pop-frame-cont b)))
                                                              (lambda (t e) (throw (pop-frame-cont t) e)))))
      ((eq? (operator l) '=) (eval-ass l state-cont))
      (else (error "err: fell thru")) )))

(define handle-try
  (lambda (expr state-cont return continue break throw)
    (if (null? (caddr expr))
         (eval-finally (cadddr expr) ; no catch
                       (pop-frame-cont (eval-begin (cadr expr) 
                                                 (push-frame-cont state-cont) 
                                                 return 
                                                 (lambda (c) (continue (pop-frame-cont c)))
                                                 (lambda (b) (break (pop-frame-cont b)))
                                                 (lambda (t e) (throw (pop-frame-cont t) e))))
                      return
                      (lambda (c) (continue (pop-frame-cont c)))
                      (lambda (b) (break (pop-frame-cont b)))
                      (lambda (t e) (throw (pop-frame-cont t) e)))       
         (eval-finally (cadddr expr) ; eval-finally on result of eval-try with eval-catch in its throw continuation
                       (push-frame-cont (call/cc
                                        (lambda (throw-here)
                                          (pop-frame-cont (eval-begin (cadr expr) 
                                                    (push-frame-cont state-cont)
                                                    return
                                                    (lambda (c) (continue (pop-frame-cont c)))
                                                    (lambda (b) (break (pop-frame-cont b)))
                                                    (lambda (t e) (throw-here (eval-begin (caddr (caddr expr)) ; eval-catch in throw continuation
                                                                                          (eval-decl (list 'var (car (cadr (caddr expr))) e) t)
                                                                                          return
                                                                                          (lambda (c) (continue (pop-frame-cont c)))
                                                                                          (lambda (b) (break (pop-frame-cont b)))
                                                                                          (lambda (t e) (throw (pop-frame-cont t) e))))))))))
                      return ; the rest of the eval-finally args
                      (lambda (c) (continue (pop-frame-cont c)))
                      (lambda (b) (break (pop-frame-cont b)))
                      (lambda (t e) (throw (pop-frame-cont t) e))))))
         
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

; eval-type functions
; evaluates an expression of a certain type

; eval-decl - evaluate variable declaration
(define eval-decl-s
  (lambda (expr state) 
      (if (null? (var-tail expr)) ; is there no value provided?
        (cons ; then declare to empty list
         (cons 
          (cons (var-name expr) (state-vars state)) ; add the name
          (cons (cons '() (state-values state)) '())) ; no value, make it the empty list
         (cdr state)) 
        (cons ; else declare to its value
         (cons 
          (cons (var-name expr) (state-vars state)) ; add the name
          (cons (cons (eval-bool-or-val (var-val expr) state) (state-values state)) '())) ; evaluate the value expression and add it
         (cdr state))
        )
      ))

(define eval-decl
  (lambda (expr state-cont)
    (lambda (x) (eval-decl-s expr (state-cont x)))))

; eval-ass - evaluate an assignment
(define eval-ass-s
  (lambda (expr state)
    (cond
      ((null? state) '())                      
      ((isVar? (ass-val expr) state) (eval-ass-s (list (operator expr) (ass-var expr) (deref (ass-val expr) state)) state)) ; when assigning a variable to a variable, deref first
      ((and (not (atom? (ass-val expr))) (value? (ass-val expr) state)) (eval-ass-s (list (operator expr) (ass-var expr) (eval-bool-or-val (ass-val expr) state)) state))
      ((not (isVar? (ass-var expr) state)) (error "Error: Attempted to assign to undeclared variable."))
      ((null? (state-vars state)) (cons (car state) (eval-ass-s expr (cdr state)))) ; base case of current state frame
      ((eq? (ass-var expr) (car (state-vars state))) ; found our variable, return the modified state
       (cons ; cons state variables onto the cons of state-values onto '()
        (cons (state-vars state) ; the state variables
              (cons 
               (cons (eval-bool-or-val (ass-val expr) state) ; add the value to the cdr of state-values
                     (cdr (state-values state)))
               '()))
        (cdr state)))
      (else 
       (cons 
        (cons 
         (cons ; state-vars of the current frame
          (car (state-vars state))
          (state-vars (eval-ass-s expr 
                                (cons 
                                 (cons 
                                  (cdr (state-vars state)) 
                                  (cons 
                                   (cdr (state-values state)) 
                                   '()))
                                 (eval-ass-s expr (cdr state))))))
         (cons ; state-values of the current frame
          (cons
           (car (state-values state))
           (state-values (eval-ass-s expr 
                                   (cons 
                                    (cons 
                                     (cdr (state-vars state)) 
                                     (cons 
                                      (cdr (state-values state)) 
                                      '()))
                                    (eval-ass-s expr (cdr state)))))) '()))
        (cdr (eval-ass-s expr ; the result of recursing on the cdr of the state
                       (cons 
                        (cons (cdr (state-vars state)) 
                              (cons 
                               (cdr (state-values state)) 
                               '()))
                        (eval-ass-s expr (cdr state))))) )))))  ; the rest of this frame

(define eval-ass
  (lambda (expr state-cont)
    (lambda (x) (eval-ass-s expr (state-cont x)))))

(define eval-begin
  (lambda (expr state-cont return continue break throw)
    (if (null? expr)
        state-cont
        (eval-begin (cdr expr) (evaluate (car expr) state-cont return continue break throw) return continue break throw))))

(define eval-if
  (lambda (expr state-cont return continue break throw)
    (if (eval-bool (if-cond expr) (state-cont e-s))
        (evaluate (caddr expr) state-cont return continue break throw)
        (if (hasThreeTerms? expr)
            state-cont
            (evaluate (if-elsex expr) state-cont return continue break throw)))))

(define eval-while
  (lambda (expr state-cont return continue break throw)
    (if (eval-bool (while-cond expr) (state-cont e-s))
        (eval-while expr 
                    (call/cc
                     (lambda (continue-here)
                       (evaluate (caddr expr) state-cont return continue-here break throw))) 
                    return
                    continue
                    break
                    throw)
        state-cont)))

(define eval-finally
  (lambda (expr state-cont return continue break throw)
    (cond 
      ((null? expr) state-cont)
      ((eq? (operator expr) 'finally) (eval-begin (cadr expr) state-cont return continue break throw))
      (else (eval-finally (cdr expr) (evaluate (car expr) state-cont return continue break throw) return continue break throw)))))

; eval-func - applies the body of a function to state and return state
; expr looks something like (f 3 4) where f is a function
(define eval-func
  (lambda (expr state-cont return continue break throw)
    (eval-begin (cadr (deref (car expr) (state-cont e-s))) ; get the body of the function
                (bind-parameters (car (deref (car expr) (state-cont e-s))) ; get the params
                                 (cdr expr) ; get the arguments
                                 state-cont)              
                return
                continue
                break
                throw)))

; bind-parameters - a recursive function which applies the arguments to parameters
(define bind-parameters
  (lambda (params args state-cont)
    (cond
      ((and (null? params) (null? args)) state-cont)
      ((null? params) (error "Too many arguments, not enough parameters!"))
      ((null? args) (error "Too many parameters, not enough arguments!"))
      (else (eval-decl (list 'var (car params) (car args)) (bind-parameters (cdr params) (cdr args) state-cont))))))
    
                
; eval-bool-or-val - evaluate an expr of bool or value (numeric) type
(define eval-bool-or-val
  (lambda (expr state)
    (cond
      ((number? expr) expr)
      ((bool? expr state) (if (eval-bool expr state) "TRUE" "FALSE"))
      ((value? expr state) (eval-value expr state))
      ((not (isVar? expr state)) (error (begin (begin (begin (display "\nErr:") (print expr)) (display state)) "Error: Attempted to dereference undeclared value")))
      (else (error "Neither bool nor value")))))

; get the value of a boolean expression
; can we use #t and #f internally, but return as a string..?
(define eval-bool
  (lambda (expr state)
    (cond
      ((number? expr) (not (zero? expr))) ; any non-zero number is considered TRUE
      ((or (eq? expr 'TRUE) (eq? expr 'true) (eq? expr "TRUE")))
      ((or (eq? expr 'FALSE) (eq? expr 'false) (eq? expr "FALSE")) #f)
      ((atom? expr) #t)
      ((eq? (operator expr) '>) (> (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '<) (< (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '>=) (>= (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '<=) (<= (eval-value (operand1 expr) state) (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '==) (eq? (eval-value (operand1 expr) state)
                                      (eval-value (operand2 expr) state)))
      ((eq? (operator expr) '!=)  (not (eq? (eval-value (operand1 expr) state) 
                                            (eval-value (operand2 expr) state))))
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
      ((ass? expr) (ass-val expr)) ; side effects?
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

(define push-frame-cont
  (lambda (state-cont)
    (lambda (x) (push-frame (state-cont x)))))

(define pop-frame-cont
  (lambda (state-cont)
    (lambda (x) (pop-frame (state-cont x)))))

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
      ((atom? expr) (or (eq? expr 'true) (eq? expr "TRUE") (eq? expr 'false) (eq? expr "FALSE")) ) ; if it's an atom, it must be 'TRUE or 'FALSE or a variable 0_0
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
       (or (and (bool? (operand1 expr) state)
                (bool? (operand2 expr) state))
           (and (value? (operand1 expr) state)
                (value? (operand2 expr) state))))
      ((eq? (operator expr) '==)
       (or (and (bool? (operand1 expr) state)
                (bool? (operand2 expr) state))
           (and (value? (operand1 expr) state)
                (value? (operand2 expr) state))))
      (else #f))))

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
