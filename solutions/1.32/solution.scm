(define (rec-sum term a next b) 
    (if (> a b) 
        0
        (+ (term a)
           (rec-sum term (next a) next b))))

(define (iter-sum term a next b) 
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result)))) 
    (iter a 0))

(define (rec-prod term a next b)
    (if (> a b)
        1
        (* (term a)
           (rec-prod term (next a) next b))))

(define (iter-prod term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

(define (rec-acc combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (rec-acc combiner null-value term (next a) next b))))

(define (iter-acc combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))))
    (iter a null-value))

(define (rec-acc-sum term a next b)
    (rec-acc + 0 term a next b))

(define (iter-acc-sum term a next b)
    (iter-acc + 0 term a next b))

(define (rec-acc-prod term a next b)
    (rec-acc * 1 term a next b))

(define (iter-acc-prod term a next b)
    (iter-acc * 1 term a next b))

(define (cube x) (* x x x))

(define (inc a) (+ a 1))

(newline)
(display "Enter a: ")
(define a (read))
(display "Enter b: ")
(define b (read))

(display "rec-sum: ")
(display (rec-sum cube a inc b))

(newline)
(display "iter-sum: ")
(display (iter-sum cube a inc b))

(newline)
(display "rec-acc-sum: ")
(display (rec-acc-sum cube a inc b))

(newline)
(display "iter-acc-sum: ")
(display (iter-acc-sum cube a inc b))

(newline)
(display "rec-prod: ")
(display (rec-prod cube a inc b))

(newline)
(display "iter-prod: ")
(display (iter-prod cube a inc b))

(newline)
(display "rec-acc-prod: ")
(display (rec-acc-prod cube a inc b))

(newline)
(display "iter-acc-prod: ")
(display (iter-acc-prod cube a inc b))
