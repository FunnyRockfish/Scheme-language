(use-syntax (ice-9 syncase))

;ачивка 3: это особенность call\cc, т.е. применяется функция к "текущему
;продолжению" выражения. Этот фокус похож на пример функции return: 
;(define (f return)
  ;(return 2) 
  ;3)

;(f (lambda (x) x))
;=> 3

;(call-with-current-continuation f)
;=> 2

(define ie (interaction-environment))
(define (s->s arg)
  (if (symbol? arg)
      (symbol->string arg)
      (string->symbol arg)))

;#1
(define memoized-factorial
  (let ((memo '()))
    (lambda (n)
      (let ((memoized (assq n memo)))
        (if (not (equal? memoized #f))
            (cadr memoized)
            (let ((new-value
                   (if (< n 1)
                       1
                       (* n (memoized-factorial (- n 1))))))
              (set! memo (cons (list n new-value) memo))
              new-value))))))

;#2
(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (delay (cons a b)))))

(define (lazy-car p)
  (car (force p)))

(define (lazy-cdr p)
  (cdr (force p)))

(define (lazy-head xs k)
  (let loop ((xs xs) (k k) (res '()))
    (if (= k 0)
        (reverse res)
        (loop (lazy-cdr xs) (- k 1) (cons (lazy-car xs) res)))))

(define (lazy-ref xs k)
  (let loop ((xs xs) (k k))
    (if (= k 0)
        (lazy-car xs)
        (loop (lazy-cdr xs) (- k 1)))))

(define (naturals n)
  (lazy-cons n (naturals (+ n 1))))

(define (factorials)
  (let loop ((p 1) (n 1))
    (lazy-cons (* p n) (loop (* p n) (+ n 1)))))

(define (lazy-factorial n)
  (list-ref (lazy-head (factorials) n)
            (- n 1)))


(define (read-words)
  (define (f words word c)
    (cond ((and (eof-object? c) (not (null? words)))
           (reverse words))
          ((and (eof-object? c) (not (null? word)))
           (f (cons (list->string (reverse word)) words) '()
              (read-char)))
          ((eof-object? c) (reverse words))
          ((and (or (equal? c #\newline) (equal? c #\tab)
                    (equal? c #\space)) (null? word)) (f words word
                                                         (read-char)))
          ((or (equal? c #\newline) (equal? c #\tab)
               (equal? c #\space)) (f (cons (list->string
                                             (reverse word)) words)
                                      '() (read-char)))
          (else (f words (cons c word) (read-char)))))
  (f '() '() (read-char)))

;#4
(define-syntax define-struct
  (syntax-rules ()
    ((_ sym-name sym-fields)
     (let loop ((name (symbol->string 'sym-name))
                (fields (map symbol->string 'sym-fields))
                (i 2))
       (if (null? fields)
           (eval `(begin (define (,(string->symbol (string-append "make-"
                                                                  name))
                                  . vals)
                           (list->vector (cons '_struct
                                               (cons 'sym-name vals))))
                         (define (,(string->symbol (string-append name
                                                                  "?"))
                                  obj)
                           (and (vector? obj)
                                (> (vector-length obj) 2)
                                (eqv? '_struct (vector-ref obj 0))
                                (eqv? 'sym-name (vector-ref obj 1)))))
                 (interaction-environment))
           (begin (eval `(begin (define (,(string->symbol
                                           (string-append name
                                                          "-"
                                                          (car fields)))
                                         obj)
                                  (vector-ref obj ,i))
                                (define (,(string->symbol
                                           (string-append "set-"
                                                          name
                                                          "-"
                                                          (car fields)
                                                          "!"))
                                         obj
                                         val)
                                  (vector-set! obj ,i val)))
                        (interaction-environment))
                  (loop name (cdr fields) (+ i 1))))))))
