(define (rec-sum term a next b) 
    (if (> a b) 
        0
        (+ (term a)
           (rec-sum term (next a) next b))))

(define (iter-sum term a next b) 
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result)))) 
    (iter a 0))

(define (cube x) (* x x x))

(define (inc a) (+ a 1))

(newline)
(display "Enter a: ")
(define a (read))
(display "Enter b: ")
(define b (read))
(display "rec-sum: ")
(display (rec-sum cube a inc b))
(newline)
(display "iter-sum: ")
(display (iter-sum cube a inc b))
