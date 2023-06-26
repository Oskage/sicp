(define (divides? a b) 
    (= (remainder b a) 0))

(define (even? a) 
    (divides? 2 a))

(define (next a)
    (if (= a 2) 3 (+ a 2)))

(define (smallest-divisor n)
    (define (find-divisor test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor (next test-divisor)))))
    (find-divisor 2))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (filtered-acc combiner null-value pred term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (if (pred a)
                               (combiner (term a) result)
                               result))))
    (iter a null-value))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sum-of-prime-squares a b)
    (filtered-acc + 0 prime? square a inc b))

(define (prod-of-positive-and-prime-to n)
    (define (prime-to? a)
        (= (gcd a n) 1))
    (filtered-acc * 1 prime-to? identity 1 inc n))

(newline)
(display "Enter a: ")
(define a (read))
(display "Enter b: ")
(define b (read))

(display "Sum of squares of primes from a to b: ")
(display (sum-of-prime-squares a b))

(newline)
(display "Product of prime to b numbers from 1 to b: ")
(display (prod-of-positive-and-prime-to b))
