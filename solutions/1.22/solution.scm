(define (divides? a b) 
    (= (remainder b a) 0))

(define (even? a) 
    (divides? 2 a))

(define (smallest-divisor n)
    (define (find-divisor test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor (+ test-divisor 1)))))
    (find-divisor 2))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (timed-prime-test n) 
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time) 
    (if (prime? n)
        (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time) 
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))

(define (search-for-primes start end)
    (cond ((> start end) (newline) 
                         (display "done"))
          (else (timed-prime-test start)
                (search-for-primes (+ start 1) end))))

(newline)
(display "Enter a start: ")
(define start (read))
(display "Enter an end: ")
(define end (read))
(search-for-primes start end)
