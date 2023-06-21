(define (even? x) 
    (= (remainder x 2) 0))

(define (fast-mult a b)
    (define (iter x a b)
        (cond ((= b 0) x)
              ((even? b) (iter x (+ a a) (/ b 2)))
              (else (iter (+ x a) a (- b 1)))))
    (iter 0 a b))

(newline)
(display "Enter a number: ")
(define a (read))
(display "Enter a multiplier: ")
(define b (read))

(display a)
(display " * ")
(display b)
(display " = ")
(display (fast-mult a b))
(newline)
