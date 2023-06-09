(define (divides? a b)
    (= (remainder b a) 0))

(define (smallest-divisor n)
    (define (find-divisor test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor (+ test-divisor 1)))))
    (find-divisor 2))

(define (prime? n)
    (= n (smallest-divisor n)))

(newline)
(display "smallest divisor of 199: ")
(display (smallest-divisor 199))
(newline)
(display "smallest divisor of 1999: ")
(display (smallest-divisor 1999))
(newline)
(display "smallest divisor of 19999: ")
(display (smallest-divisor 19999))
(newline)