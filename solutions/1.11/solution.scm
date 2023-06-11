(define (recursive-f n)
    (if (< n 3)
        n
        (+ (recursive-f (- n 1))
           (recursive-f (- n 2))
           (recursive-f (- n 3)))))

(define (iterative-f n)
    (define (f-iter a b c count)
        (if (= count 0)
            c
            (f-iter (+ a b c) a b (- count 1))))
    (f-iter 2 1 0 n))

(newline)
(display "Enter a number: ")
(define x (read))

(display "iterative f: ")
(display (iterative-f x))
(newline)

(display "recursive f: ")
(display (recursive-f x))
(newline)