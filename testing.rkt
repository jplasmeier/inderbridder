(load "interpreter.rkt")

(string-append "test 1: " (if (eq? (interpret-file "tests/test1.txt" ) 150) "passed" "failed"))
(string-append "test 2: " (if (eq? (interpret-file "tests/test2.txt" ) -4) "passed" "failed")) ; wrong value -5
(string-append "test 3: " (if (eq? (interpret-file "tests/test3.txt" ) 10) "passed" "failed"))
(string-append "test 4: " (if (eq? (interpret-file "tests/test4.txt" ) 16) "passed" "failed")) ; wrong value -17.5
(string-append "test 5: " (if (eq? (interpret-file "tests/test5.txt" ) 220) "passed" "failed"))
(string-append "test 6: " (if (eq? (interpret-file "tests/test6.txt" ) 5) "passed" "failed"))
(string-append "test 7: " (if (eq? (interpret-file "tests/test7.txt" ) 6) "passed" "failed"))
(string-append "test 8: " (if (eq? (interpret-file "tests/test8.txt" ) 10) "passed" "failed"))
(string-append "test 9: " (if (eq? (interpret-file "tests/test9.txt" ) 5) "passed" "failed"))
(string-append "test 10: " (if (eq? (interpret-file "tests/test10.txt" ) -39) "passed" "failed"))
;(string-append "test 11: " (if (eq? (interpret-file "tests/test11.txt" ) (error)) "passed" "failed"))
;(string-append "test 12: " (if (eq? (interpret-file "tests/test12.txt" ) (error)) "passed" "failed"))
;(string-append "test 13: " (if (eq? (interpret-file "tests/test13.txt" ) (error)) "passed" "failed"))
;(string-append "test 14: " (if (eq? (interpret-file "tests/test14.txt" ) (error)) "passed" "failed"))
(string-append "test 15: " (if (eq? (interpret-file "tests/test15.txt" ) "TRUE") "passed" "failed")) ; #t instead of TRUE
(string-append "test 16: " (if (eq? (interpret-file "tests/test16.txt" ) 100) "passed" "failed"))
(string-append "test 17: " (if (eq? (interpret-file "tests/test17.txt" ) "FALSE") "passed" "failed")) ; #f
(string-append "test 18: " (if (eq? (interpret-file "tests/test18.txt" ) "TRUE") "passed" "failed")) ; #t
(string-append "test 19: " (if (eq? (interpret-file "tests/test19.txt" ) 128) "passed" "failed")) ; 4
(string-append "test 20: " (if (eq? (interpret-file "tests/test20.txt" ) 12) "passed" "failed")) ; -19
(string-append "test 21: " (if (eq? (interpret-file "tests/test21.txt" ) 30) "passed" "failed"))
(string-append "test 22: " (if (eq? (interpret-file "tests/test22.txt" ) 11) "passed" "failed"))
(string-append "test 23: " (if (eq? (interpret-file "tests/test23.txt" ) 1106) "passed" "failed"))
(string-append "test 24: " (if (eq? (interpret-file "tests/test24.txt" ) 12) "passed" "failed"))
(string-append "test 25: " (if (eq? (interpret-file "tests/test25.txt" ) 16) "passed" "failed"))
(string-append "test 26: " (if (eq? (interpret-file "tests/test26.txt" ) 72) "passed" "failed"))
(string-append "test 27: " (if (eq? (interpret-file "tests/test27.txt" ) 21) "passed" "failed"))
(string-append "test 28: " (if (eq? (interpret-file "tests/test28.txt" ) 164) "passed" "failed"))
