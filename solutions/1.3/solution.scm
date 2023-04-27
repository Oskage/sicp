(define (sum_of_squares x y) (+ (* x x) (* y y)))

(define (<= x y) (not (> x y)))

(define (foo a b c)
    (cond ((and (<= a c) (<= a b)) (sum_of_squares b c))
          ((and (<= b a) (<= b c)) (sum_of_squares a c))
          ((and (<= c a) (<= c b)) (sum_of_squares a b)))) 

(newline)
(define a (read))
(define b (read))
(define c (read))

(display (foo a b c))
(newline)

