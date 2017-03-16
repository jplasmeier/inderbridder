(load "interpreter.rkt")
(load "testframework.rkt")

; Unit Tests

; Helper Function Tests

(test-me-list listOfOne?
              '(((a)) ((a b)) (()) ((())))
              '(#t #f #f #t)
              '("list of one" "list of two" "null list" "list of null"))

(test-me-list atom?
              '((a) (()) ((a)))
              '(#t #f #f)
              '("atom" "null list" "list of atom"))

(test-me-list member*
              '((a (a b c)) (b (a d c)) (d (a (b (c d) e))) (a ()))
              '(#t #f #t #f)
              '("atom in flat" "not in flat" "in nested" "in null"))

(test-me-list isUnary?
              '(((- 5)) ((- 5 4)) ((-)))
              '(#t #f #f)
              '("unary" "binary" "0ary"))

(test-me-list hasThreeTerms?
              '(((- 5)) ((- 5 4)) ((-)) ((a b c d)))
              '(#f #t #f #f)
              '("two terms" "three terms" "one term" "four terms"))

; value? tests
(if (and
(eq? (value? '(- (/ (* 6 (+ 8 (% 5 3))) 11) 9) '(()())) #t)
)
(display "value? tests pass\n")
(display "value? tests fail\n"))

;(eq? (decl? '(var x (/ (- (* 5 7) 3) 2))) #t)
(test-me-list decl? 
              '(((var x) (()())) ((var x y) (()())) ((var 3) (()())) ((x = 3) (()())) ((x - 2) (()())) ((x y) (()()))) 
              '(#t #f #f #f #f #f) 
              '("declare one var" "declare two vars" "declare int" "declare assign" "declare arith" "declare two atoms"))

; eval-decl tests
(test-me-list eval-decl 
              '(((var x) (()())) ((var y) (()()))) 
              '(((x)(())) ((y)(()))) 
              '("declare x" "declare y"))

; ass? tests
(test-me-list ass? 
              '(((= x 3) ((x)(()))) ((x y z) ((x)(()))) ((= y 3) ((x)(()))) ((x y) ((x)(()))) ((var x) ((x)(()))))
              '(#t #f #f #f #f)
              '("ass? valid" "ass? three var" "ass? noninit" "ass? two var" "ass? declare"))

; while? tests
(test-me-list while?
              '(((while (< x 100) (= x (* x 2))) ((x)(2))))
              '(#t)
              '("valid while"))
              
; deref tests
(test-me deref '(y ((x y z) (() 6 8))) 6 "deref test")

; eval-ass tests
(if (and 
(eq? (deref 'x (eval-ass '(= x 4) '((x) (())))) 4)
(eq? (deref 'x (eval-ass '(= x 5) '((x y z) (() 6 8)))) 5)
(eq? (deref 'y (eval-ass '(= y 5) '((x y z) (() 6 8)))) 5)
(eq? (deref 'z (eval-ass '(= z 1) '((x y z) (() 3 4)))) 1)
(eq? (deref 'x (eval-ass '(= x 3) '((y x) (() ())))) 3)
;(eq? (deref 'y (eval-ass '(= x (= y 10)) '((x y)(() ())))) 10)
)
(display "eval-ass tests pass\n")
(display "eval-ass tests fail\n"))

; eval-value tests
(test-me-list eval-value 
              '(((* 6 (+ 8 (% 5 3))) (()())) ((/ (* 6 (+ 8 (% 5 3))) 11) (()()))  ((- (/ (* 6 (+ 8 (% 5 3))) 11) 9) (()())))
              '(60 5 -4)
              '("ppretest case 2" "pretest case 2" "test case 2"))

; isUnary? tests
(test-me-list isUnary?
              '(((- 5 4)) ((- 5)))
              '(#f #t)
              '("non unary" "unary"))

; bool? tests
(define BOOLSTATENONE '(()()))
(if (and
(eq? (bool? '(< 45 50) BOOLSTATENONE) #t)
(eq? (bool? '(< 45 5) BOOLSTATENONE) #t)
(eq? (bool? '(> 45 50) BOOLSTATENONE) #t)
(eq? (bool? '(> 45 5) BOOLSTATENONE) #t)
(eq? (bool? '(== 40 40) BOOLSTATENONE) #t)
(eq? (bool? '(== 40 41) BOOLSTATENONE) #t)
(eq? (bool? '(!= 40 41) BOOLSTATENONE) #t)
(eq? (bool? '(!= 40 40) BOOLSTATENONE) #t)
(eq? (bool? '(<= 40 41) BOOLSTATENONE) #t)
(eq? (bool? '(<= 41 41) BOOLSTATENONE) #t)
(eq? (bool? '(<= 42 41) BOOLSTATENONE) #t)
(eq? (bool? '(>= 40 41) BOOLSTATENONE) #t)
(eq? (bool? '(>= 41 41) BOOLSTATENONE) #t)
(eq? (bool? '(>= 42 41) BOOLSTATENONE) #t)
(eq? (bool? '(> (+ 5 3) (+ 2 4)) BOOLSTATENONE) #t)
(eq? (bool? '(! true) BOOLSTATENONE) #t)
(eq? (bool? '(! false) BOOLSTATENONE) #t)
(eq? (bool? '(|| true false) BOOLSTATENONE) #t)
(eq? (bool? '(|| true true) BOOLSTATENONE) #t)
(eq? (bool? '(|| false true) BOOLSTATENONE) #t)
(eq? (bool? '(|| false false) BOOLSTATENONE) #t)
(eq? (bool? '(&& false false) BOOLSTATENONE) #t)
(eq? (bool? '(&& false true) BOOLSTATENONE) #t)
(eq? (bool? '(&& true false) BOOLSTATENONE) #t)
(eq? (bool? '(&& true true) BOOLSTATENONE) #t)
(eq? (bool? '(&& (> 5 2) true) BOOLSTATENONE) #t)
(eq? (bool? '(== true true) BOOLSTATENONE) #t)
(eq? (bool? '(! TRUE) BOOLSTATENONE) #t)
(eq? (bool? '(! FALSE) BOOLSTATENONE) #t)
(eq? (bool? '(|| TRUE FALSE) BOOLSTATENONE) #t)
(eq? (bool? '(|| TRUE TRUE) BOOLSTATENONE) #t)
(eq? (bool? '(|| FALSE TRUE) BOOLSTATENONE) #t)
(eq? (bool? '(|| FALSE FALSE) BOOLSTATENONE) #t)
(eq? (bool? '(&& FALSE FALSE) BOOLSTATENONE) #t)
(eq? (bool? '(&& FALSE TRUE) BOOLSTATENONE) #t)
(eq? (bool? '(&& TRUE FALSE) BOOLSTATENONE) #t)
(eq? (bool? '(&& TRUE TRUE) BOOLSTATENONE) #t)
(eq? (bool? '(&& (> 5 2) TRUE) BOOLSTATENONE) #t)
(eq? (bool? '(== TRUE TRUE) BOOLSTATENONE) #t)
;(eq? (bool? '(== true 30) BOOLSTATENONE) #f) this gives an error- probably should anyway
)
(display "\nbool? tests pass\n")
(display "bool? tests fail\n"))

; eval-bool tests
(if (and
(eq? (eval-bool '(< 45 50) '(()())) "TRUE")
(eq? (eval-bool '(< 45 5) '(()())) "FALSE")
(eq? (eval-bool '(> 45 50) '(()())) "FALSE")
(eq? (eval-bool '(> 45 5) '(()())) "TRUE")
(eq? (eval-bool '(== 40 40) '(()())) "TRUE")
(eq? (eval-bool '(== 40 41) '(()())) "FALSE")
(eq? (eval-bool '(!= 40 41) '(()())) "TRUE")
(eq? (eval-bool '(!= 40 40) '(()())) "FALSE")
(eq? (eval-bool '(<= 40 41) '(()())) "TRUE")
(eq? (eval-bool '(<= 41 41) '(()())) "TRUE")
(eq? (eval-bool '(<= 42 41) '(()())) "FALSE")
(eq? (eval-bool '(>= 40 41) '(()())) "FALSE")
(eq? (eval-bool '(>= 41 41) '(()())) "TRUE")
(eq? (eval-bool '(>= 42 41) '(()())) "TRUE")
(eq? (eval-bool '(> (+ 5 3) (+ 2 4)) '(()())) "TRUE")
(eq? (eval-bool '(! true) '(()())) "FALSE")
(eq? (eval-bool '(! false) '(()())) "TRUE")
;(eq? (eval-bool '(|| true false) '(()())) "TRUE")
;(eq? (eval-bool '(|| true true) '(()())) "TRUE")
;(eq? (eval-bool '(|| false true) '(()())) "TRUE")
;(eq? (eval-bool '(|| false false) '(()())) "FALSE")
;(eq? (eval-bool '(&& false false) '(()())) "FALSE")
;(eq? (eval-bool '(&& false true) '(()())) "FALSE")
;(eq? (eval-bool '(&& true false) '(()())) "FALSE")
;(eq? (eval-bool '(&& true true) '(()())) "TRUE")
;(eq? (eval-bool '(&& (> 5 2) true) '(()())) "TRUE")
;(eq? (eval-bool '(== false false) '(()())) "TRUE")
;(eq? (eval-bool '(!= false true) '(()())) "TRUE")
)
(display "eval-bool tests pass\n")
(display "eval-bool tests fail\n"))

(if (and
(eq? (if? '(if (> x y) (= x 3)) '((x y)(6 4))) #t)
)
(display "if tests pass\n")
(display "if tests fail\n"))

; eval-if tests
(test-me-list eval-if
              '(((if (<= x y) (= m x) (= m y)) ((m y x) (() 6 5))) ((if (<= x y) (= m x) (= m y)) ((m y x) (() 5 6))))
              '(((m y x) (5 6 5)) ((m y x) (5 5 6)))
              '("if with else true" "if with else false"))

; return? tests
(if (and
(return? '(return x) '((x)(2)))
)
(display "\nreturn tests pass\n")
(display "return tests fail\n"))

; integration tests
;(define state '(()()))
;(define state (interpret '((var x) (var y) (= y 5) (= x 2) (if (> y x) ((var ifvar) (= ifvar 40)))) state))
;(interpret '((= x 3)) state)

;(display "integration tests pass\n")
;(display "integration tests fail\n")
