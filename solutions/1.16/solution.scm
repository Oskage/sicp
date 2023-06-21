(define (even? x) 
    (= (remainder x 2) 0))

(define (fast-exp-rec b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-exp-rec b (/ n 2))))
          (else (* b (fast-exp-rec b (- n 1))))))

(define (fast-exp-iter b n)
    (define (iter a b n)
        (cond ((= n 0) a)
              ((even? n) (iter a (square b) (/ n 2)))
              (else (iter (* a b) b (- n 1)))))
    (iter 1 b n))

(newline)
(display "Enter a base: ")
(define b (read))
(display "Enter a power: ")
(define n (read))

(display "iterative : ")
(display (fast-exp-iter b n))
(newline)

(display "recursive : ")
(display (fast-exp-rec b n))
(newline)