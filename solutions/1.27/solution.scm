(define (even? a) (= (remainder a 2) 0))

(define (expmod base exp m) 
    (cond ((= exp 0) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) 
                                  m))
          (else (remainder (* base (expmod base (- exp 1) m)) 
                           m))))

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (carmichael-number? n)
    (define (try a)
        (if (= a 1)
            #t 
            (if (not (fermat-test n a))
                #f 
                (try (- a 1)))))
    (try (- n 1)))

(newline)
(display "Enter a number to full fermat test: ")
(define n (read))
(display (carmichael-number? n))
