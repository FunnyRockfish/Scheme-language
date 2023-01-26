(define (del-left lst)
    (cond ((null? lst) '())
          ((or (equal? (car lst) '#\tab) (equal? (car lst) '#\space)
               (equal? (car lst) '#\newline)) (del-left (cdr lst))) ;сложность O(n)
          (#t lst)))
(define (string-trim-left s)
       (list->string (del-left (string->list s))))
(define (string-trim-right s)
       (list->string (reverse (del-left (reverse (string->list s))))))
(define (string-trim s)
    (string-trim-right (string-trim-left s)))   



(define (string-reverse str) 
  (list->string (reverse (string->list str))))



(define (string-prefix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((equal? a (substring b 0 (string-length a))) #t)
        (else #f)))


(define (string-suffix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((equal? a (substring b (- (string-length b) (string-length a)))) #t)
        (else #f)))

(define (string-infix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((string-prefix? a b) #t)
        (else (string-infix? a (substring b 1)))))

(define (add-element s sep)
  (cond
    ((= (string-length s) 0) (string))
    ((string-prefix? sep s) (string))
    (else (string-append (make-string 1 (string-ref s 0)) (add-element (substring s 1) sep)))))

(define (string-split s sep)
  (cond
    ((= (string-length s) 0) (list))
    ((string-prefix? sep s) (string-split (substring s
                                                     (string-length sep))
                                          sep))
    (else (cons (add-element s sep) (string-split
                                     (substring s (string-length
                                                   (add-element s sep)))
                                     sep)))))
