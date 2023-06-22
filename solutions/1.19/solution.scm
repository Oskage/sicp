(define (even? x) 
    (= (remainder x 2) 0))

(define (fib n)
    (define (iter a b p q count)
        (cond ((= count 0) b)
              ((even? count) (iter a 
                                   b 
                                   (+ (* p p) (* q q)) 
                                   (+ (* 2 p q) (* q q))
                                   (/ count 2)))
              (else (iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
    (iter 1 0 0 1 n))

(newline)
(display "Enter a number: ")
(define n (read))
(display "fib(")
(display n)
(display ") = ")
(display (fib n))
(newline)