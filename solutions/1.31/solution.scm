(define (rec-prod term a next b)
    (if (> a b)
        1
        (* (term a)
           (rec-prod term (next a) next b))))

(define (iter-prod term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial n)
    (iter-prod identity 1 inc n))

(define (compute-pi)
    (define (term x) 
        (/ (* (- x 1) (+ x 1)) 
           (* x x)))
    (define (next x) (+ x 2))
    (* (iter-prod term 3 next 1000) 4))

(define pi (compute-pi))

(define (cube x) (* x x x))

(define (inc a) (+ a 1))

(newline)
(display "Enter a: ")
(define a (read))
(display "Enter b: ")
(define b (read))
(display "rec-prod: ")
(display (rec-prod cube a inc b))
(newline)
(display "iter-prod: ")
(display (iter-prod cube a inc b))

(newline)
(display "Enter n for factorial: ")
(define n (read))
(display "Factorial(n): ")
(display (factorial n))

(newline)
(display "pi=")
(display (exact->inexact pi))