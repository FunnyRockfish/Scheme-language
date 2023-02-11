(define (o . xs)
  (lambda (x)
    (if (null? xs)
        x
        ((car xs) ((apply o (cdr xs)) x)))))

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))
