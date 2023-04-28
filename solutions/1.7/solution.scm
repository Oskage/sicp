(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess prev-guess)
    (< (if (< guess prev-guess)
           (/ prev-guess guess)
           (/ guess prev-guess)) 
       1.000001))

(define (sqrt-iter guess prev-guess x)
    (if (good-enough? guess prev-guess)
        guess
        (sqrt-iter (improve guess x) guess x)))

(define (sqrt x) (sqrt-iter 1.0 0.0 x))

(newline)
(define x (read))
(display (sqrt x))
(newline)

