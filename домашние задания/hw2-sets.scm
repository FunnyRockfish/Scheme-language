(define (list->set xs)
  (if (null? xs)
      '()
      (if (member (car xs) (cdr xs))
          (list->set (cdr xs))
          (cons (car xs) (list->set (cdr xs))))))


(define (set? xs)
  (equal? xs (list->set xs)))


(define (union xs ys)
  (list->set (append xs ys)))


(define (intersection xs ys)
  (if (null? xs)
      '()
      (if (member (car xs) ys)
          (cons (car xs) (intersection (cdr xs) ys))
          (intersection (cdr xs) ys))))

(define (my-element? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (my-element? x (cdr xs)))))


(define (difference xs ys)
  (cond
    ((null? xs) '())
    ((my-element? (car xs) ys) (difference (cdr xs) ys))
    (else (cons (car xs) (difference (cdr xs) ys)))))


(define (symmetric-difference xs ys)
  (difference (union xs ys) (intersection xs ys)))


(define (set-eq? xs ys)
  (and (set? xs) (set? ys)
       (equal? (symmetric-difference xs ys) '())))
