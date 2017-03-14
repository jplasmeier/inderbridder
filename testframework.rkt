; Test Framework for Racket - Pretty Big

; test a function
(define test-me-s
  (lambda (func args expected message)
    (string-append 
      (if (equal? (apply func args) expected)
          "\nPassed: "
          "\nFailed: ")
      message  
      )))

(define test-me
  (lambda (func args expected message)
    (display (test-me-s func args expected message))))

; test a list of test cases
; will break and print the first failure which is subideal
(define test-me-list-s
  (lambda (func argl expl msgl)
    (cond 
      ((null? argl) "")
      ((equal? (apply func (car argl)) (car expl)) (string-append 
                                                               (string-append "\nTest: " (format "~s" (car msgl)) " passed.") 
                                                               (test-me-list-s func (cdr argl) (cdr expl) (cdr msgl))
                                                               ))
      (else (string-append 
             (string-append "\nTest: " (car msgl) " failed. Expected: " (format "~s" (car expl)) " but got: " (format "~s" (apply func (car argl))))
             (test-me-list-s func (cdr argl) (cdr expl) (cdr msgl))
             )))))

(define test-me-list
  (lambda (func argl expl msgl)
    (display (test-me-list-s func argl expl msgl))))
            

; test a cps function
(define test-cps-me
  (lambda (func args cont expected message)
    (string-append 
     (if (equal? (apply func (append args (cons cont '()))) expected) ; bit of a hack to allow lambda's to be passed without quote from apply
          "Passed: "
          "Failed: ") 
     message  
      )))