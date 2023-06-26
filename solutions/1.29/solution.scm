(define (even? a) (= (remainder a 2) 0))

(define (inc a) (+ a 1))

(define (sum term a next b) 
    (if (> a b) 
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (integral f a b dx)
    (define (add-dx x) 
        (+ x dx))
    (* (sum f (+ a (/ dx 2)) add-dx b) 
       dx))

(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    (define (y-k k) 
        (f (+ a (* k h))))
    (define (term k)
        (if (even? k)
            (* 2 (y-k k))
            (* 4 (y-k k))))
    (* (/ h 3)
       (+ (y-k 0)
          (y-k n)
          (sum term 1 inc (- n 1)))))

(define (cube x) (* x x x))

(newline)
(display "Enter dx: ")
(define dx (read))
(display "Standard integral: ")
(display (integral cube 0 1 dx))

(newline)
(display "Enter n: ")
(define n (read))
(display "Simpson rule integral: ")
(display (exact->inexact (simpson-integral cube 0 1 n)))
