(define (improve guess x)
    (/ (+ (/ x (square guess))
          (* 2 guess)) 
       3))

(define (good-enough? guess prev-guess)
    (< (if (< guess prev-guess)
           (/ prev-guess guess)
           (/ guess prev-guess)) 
       1.000001))

(define (cbrt-iter guess prev-guess x)
    (if (good-enough? guess prev-guess)
        guess
        (cbrt-iter (improve guess x) guess x)))

(define (cbrt x) (cbrt-iter 1.0 0.0 x))

(newline)
(define x (read))
(display (cbrt x))
(newline)

