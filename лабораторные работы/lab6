% Лабораторная работа № 6. Основы синтаксического и лексического анализа
% 29 ноября 2022 г.
% Артём Черников, ИУ9-12Б


# Цель работы
Хочу обрести навыки реализации лексических анализаторов и нисходящих
синтаксических анализаторов, которые используют метод рекурсивного спуска. 
Что-то подобное мы делали на рубежном контроле, 
но тут масштаб намного больше. Будем пробовать!


# Реализация 
```scheme
(define (sign-int? str)
  (letrec ((numb '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
           (sign '(#\+ #\-))
           (loop (lambda (index)
                   (cond ((< index (- (string-length str) 1))
                          (let ((elem (string-ref str index)))
                            (and (or (and (= 0 index) (member elem sign))
                                     (member elem numb))
                                 (loop (+ 1 index)))))
                         ((= index (- (string-length str) 1))
                          (and (member (string-ref str index) numb) #t))))))
    (loop 0)))

(define (no-sign-int? str)
  (letrec ((numb '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
           (loop (lambda (index)
                   (cond ((< index (- (string-length str) 1))
                          (let ((elem (string-ref str index)))
                            (and (member elem numb)
                                 (loop (+ 1 index)))))
                         ((= index (- (string-length str) 1))
                          (and (member (string-ref str index) numb) #t))))))
    (loop 0)))

(define (empty? str)
  (equal? str ""))

(define (check-frac str)
  (letrec ((loop (lambda (index)
                   (cond ((< index (- (string-length str) 1))
                          (let ((elem (string-ref str index)))
                            (if (equal? elem #\/)
                                index
                                (loop (+ 1 index)))))
                         ((= index (- (string-length str) 1))
                          (let ((elem (string-ref str index)))
                            (and (equal? elem #\/) index)))))))
    (let ((ind-div (loop 0)))
      (and ind-div (not (empty? str))
           (let* ((len (string-length str))
                  (head (substring str 0 ind-div))
                  (tail (substring str (+ 1 ind-div) len)))
             (and (not (empty? head)) (not (empty? tail))
                  (sign-int? head) (no-sign-int? tail)))))))

        
;1.2
(define (scan-frac str)
  (let ((is-numb (check-frac str)))
    (and is-numb (string->number str))))

;1.3
(define (scan-many-fracs str)
  (letrec ((len (string-length str))
           (loop (lambda (index start)
                   (cond ((< index (- len 1))
                          (let ((elem (string-ref str index)))
                            (cond ((and (char-whitespace? elem) (> start -1))
                                   (let ((tail (loop (+ 1 index) -1)))
                                     (and (check-frac
                                           (substring str start index))
                                          tail
                                          (cons (scan-frac
                                                 (substring str start index))
                                                tail))))
                                  ((char-whitespace? elem) (loop (+ 1 index) -1))
                                  ((> start -1) (loop (+ 1 index) start))
                                  ((= start -1) (loop (+ 1 index) index)))))
                         ((= index (- len 1))
                          (let ((elem (string-ref str index)))
                            (cond ((and (char-whitespace? elem) (> start -1))
                                   (and (check-frac (substring str start index))
                                        (list (scan-frac
                                               (substring str start index)))))
                                  ((char-whitespace? elem) '())
                                  ((> start -1)
                                   (and (check-frac (substring str start
                                                               (+ 1 index)))
                                        (list (scan-frac
                                               (substring str start
                                                                    (+ 1 index))))))
                                  ((= start -1) #f))))
                         (else '())))))
    (let ((result (loop 0 -1)))
      result)))




;2 часть
;поток stream
(define (make-stream vect)
  (list 'stream 0 vect))

(define (peek stream)
  (let* ((vect (caddr stream))
         (index (cadr stream))
         (len (vector-length vect)))
    (if (= index len)
        'end-stream
        (vector-ref vect index))))

(define (peek2 stream)
  (let* ((vect (caddr stream))
         (index (cadr stream))
         (len (vector-length vect)))
    (cond ((= index (- len 1))
           (list (vector-ref vect index) 'end-stream))
          ((= index len) (list 'end-stream 'end-stream))
          (else (list (vector-ref vect index) (vector-ref vect
                                                          (+ 1 index)))))))

(define (next stream)
  (let* ((vect (caddr stream))
         (index (cadr stream))
         (len (vector-length vect)))
    (if (= index len)
        'end-stream
        (begin
          (set-cdr! stream (list (+ 1 index) vect))
          (vector-ref vect index)))))
  


(define special-words
  '(mod neg drop swap dup over rot depth / + - * = > < not and or exit))
(define keys '(if endif define end))


(define (parse-finish stream error)
  (let ((el (peek stream)))
    (equal? el 'end-stream)))

(define (parse-program stream error)
  (let ((articles (parse-articles stream error))
        (body (parse-body stream error))
        (finish (parse-finish stream error)))
    (and (and articles body finish)
         (list articles body))))
           
(define (parse-articles stream error)
  (let* ((article (parse-article stream error))
         (articles (and
                    (not (equal? article '()))
                    article
                    (parse-articles stream error))))
    (if (and article articles (not (equal? article '())))
        (cons article articles)
        article)))
;define
(define (parse-define stream error)
  (let ((el (peek stream)))
    (and (equal? el 'define)
         (next stream))))
;end
(define (parse-end stream error)
  (let ((el (peek stream)))
    (and (equal? el 'end)
         (next stream))))
;word
(define (parse-word stream error)
  (let ((el (peek stream)))
    (and (symbol? el) (next stream))))
    
(define (parse-article stream error)
  (let* ((def (parse-define stream error))
         (word (and def (parse-word stream error))))
    (set-cdr! special-words (cons word (cdr special-words)))

    (let*((body (and word (parse-body stream error)))
          (end (and body (parse-end stream error))))
      (if def
          (and def word body end (list word body))
          '()))))
;endif
(define (parse-endif stream error)
  (let ((el (peek stream)))
    (and (equal? el 'endif) (next stream))))

(define (word? el)
  (or (member el special-words) (and (symbol? el)
                                     (not (number? el))
                                     (not (equal? el 'end-stream))
                                     (not (boolean? el))
                                     (not (member el keys)))))

(define (parse-body stream error)
  (let ((first (peek stream)))
    (cond
      ((equal? first 'if)
       (next stream)
       (let* ((body (parse-body stream error))
              (endif (and body (parse-endif stream error)))
              (body-next (and endif (parse-body stream error))))
         (and first body endif body-next
              (append (list (append (list first body))) body-next))))
      
      ;integer <Body>
      ((number? first)
       (next stream)
       (let ((body (parse-body stream error)))
         (and first body (cons first body))))
      ;word <Body>
      ((word? first)
       (next stream)
       (let ((body (parse-body stream error)))
         (and first body (cons first body))))
      ;else
      (else '()))))


(define (parse vect)
  (letrec ((stream (make-stream vect)))       
    (call-with-current-continuation
     (lambda (error)
       (let ((result (parse-program stream error)))
         (and (equal? (peek stream) 'end-stream) result))))))
```


# Тестирование
```
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (check-frac "110/111")
#t
> (check-frac "FF/10")
#f
> (scan-frac "110/111")
110/111
> (scan-frac "+5/10")
1/2
> (scan-frac "FF/10")
#f
> (scan-many-fracs
 "\t1/2 1/3\n\n10/8")
(1/2 1/3 1 1/4)
> (scan-many-fracs
 "\t1/2 1/3\n\n2/-5")
#f
> (parse #(1 2 +))
(() (1 2 +))
> (parse #(x dup 0 swap if drop -1 endif))
(() (x dup 0 swap (if (drop -1))))
> (parse #( define -- 1 - end
          define =0? dup 0 = end
          define =1? dup 1 = end
          define factorial
              =0? if drop 1 exit endif
              =1? if drop 1 exit endif
              dup --
              factorial
              *
          end
          0 factorial
          1 factorial
          2 factorial
          3 factorial
          4 factorial ))
(((-- (1 -)) (=0? (dup 0 =)) (=1? (dup 1 =)) 
(factorial (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) 
dup -- factorial *))) 
(0 factorial 1 factorial 2 factorial 3 factorial 4 factorial))
>  (((-- (1 -))
```

# Вывод
Ну что ж, я научился реализовывать нисходящий рекурсивный парсер 
не просто в БНФ грамматике, а уже на примере настоящего 
функционального языка программирования. 
Было также интересно создать простейшие сканеры.
