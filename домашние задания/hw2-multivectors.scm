(define (make-multi-vector sizes . fill) 
  (cons
   sizes
   (list (if (= (length fill) 1) 
             (make-vector (apply * sizes) (car fill))
             (make-vector (apply * sizes))))))


(define m (make-multi-vector '(1 2 3) 1))


(define (multi-vector? m)
  (and (list? m) (list? (car m)) (vector? (cadr m))))

(define (pos-in-vector sizes indices)
  (if (null? (cdr sizes))
      (car sizes)
      (+ (* (car indices) (apply * (cdr sizes)))
         (pos-in-vector (cdr sizes) (cdr indices)))))

(define (multi-vector-ref m indices)
  (vector-ref (cadr m) (pos-in-vector (car m) indices)))

(define m (make-multi-vector '(3 3 2 2) 1))

(define (multi-vector-set! m indices x)
  (vector-set! (cadr m) (pos-in-vector (car m) indices) x))
