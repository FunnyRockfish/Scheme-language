% Лабораторная работа № 3. Типы данных. Модульное тестирование
% 10 октября 2022 г.
% Черников Артём, ИУ9-12Б


# Цель работы: познакомиться с таким процессом, как юнит-тестирование, 
который пригодится мне и в дальнейшем; познакомиться на практике 
с системой типов в языке Scheme (изучали пока только на лекции);
Разработать свои собственные средства отладки, чтобы именно их и 
использовать в своих будущих программах; понять на практике, 
что такое метапрограммирование языка Scheme  и с чем это едят.

# Реализация
```scheme
;; #1. Реализуйте макрос trace-ex для трассировки
(load "trace-ex.scm")

(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss))))
      `()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))

;; #1. Tests
(zip '(1 2 3) '(one two three))


;; #2. Юнит-тестирование
; Пример процедуры с ошибкой
(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0)  1) ; Ошибка здесь!
    (else     1)))

; Загружаем каркас
(load "unit-test.scm")

; Определяем список тестов
(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

;; #3. Взятие элемента последовательности по индексу. Вставка элемента
;; в последовательность по индексу
(define (ref object index . value)
  (define (list-head xs index)
    `(,@(reverse (list-tail (reverse xs) (- (length xs) index)))))
  (define (list-insert xs index p)
    `(,@(list-head xs index) ,(car p) ,@(list-tail xs index)))
  (if (null? value)
      (cond
        ((list? object) (and (< index (length object)) (list-ref object index)))
        ((vector? object) (and (< index (vector-length object)) (vector-ref object index)))
        ((string? object) (and (< index (string-length object)) (string-ref object index)))
        (else #f))
      (cond
        ((list? object) (and (<= index (length object)) (list-insert object index value)))
        ((vector? object) (and (<= index (vector-length object))
                               (list->vector (list-insert (vector->list object) index value))))
        ((string? object) (and (<= index (string-length object))
                               (char? (car value)) (list->string
                                                    (list-insert
                                                     (string->list object)
                                                     index value))))
        (else #f))))

;; #3. Tests
(define tests3
  (list
   (test (ref '(1 2 3) 1) 2)
   (test (ref #(1 2 3) 1) 2)
   (test (ref "123" 1) #\2)
   (test (ref "123" 3) #f)
   (test (ref '(1 2 3) 1 0) `(1 0 2 3))
   (test (ref #(1 2 3) 1 0) #(1 0 2 3))
   (test (ref #(1 2 3) 1 #\0) #(1 #\0 2 3))
   (test (ref "123" 1 #\0) "1023")
   (test (ref "123" 1 0) #f)
   (test (ref "123" 3 #\4) "1234")
   (test (ref "123" 5 #\4) #f)
   ))



;; #4. Разложение на множители
(define (factorize e)
  (let ((x (cadadr e)) (y (car (cdaddr e))))
    (cond ((and (= (car (cddadr e)) 2) (= (car (cdr (cdaddr e))) 2))
           `(* (- ,x ,y) (+ ,x ,y)))
          ((and (= (car (cddadr e)) 3) (= (car (cdr (cdaddr e))) 3))
           (if (equal? (car e) '-)
               `(* (- ,x ,y) (+ (expt ,x 2) (* ,x ,y) (expt ,y 2)))
               `(* (+ ,x ,y) (- (+ (expt ,x 2) (expt ,y 2)) (* ,x ,y))))))))

;; #4. Tests
(define tests4
  (list
   (test (factorize '(- (expt x 2) (expt y 2))) `(* (- x y) (+ x y)))
   (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2)))
         `(* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1))))
   (test (eval (list (list 'lambda 
                           '(x y) 
                           (factorize '(- (expt x 2) (expt y 2))))
                     1 2)
               (interaction-environment)) -3)))
(run-tests the-tests)
(run-tests tests3)
(run-tests tests4)
```

# Тестирование
```
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
(car xss) => (1 2 3)
xss => ((1 2 3) (one two three))
(car xss) => (2 3)
xss => ((2 3) (two three))
(car xss) => (3)
xss => ((3) (three))
(car xss) => ()
((1 one) (2 two) (3 three))
(signum -2) OK
(signum 0) FAIL
  Expected: 0
  Result:   1
(signum 2) OK
#f
(ref '(1 2 3) 1) OK
(ref #(1 2 3) 1) OK
(ref "123" 1) OK
(ref "123" 3) OK
(ref '(1 2 3) 1 0) OK
(ref #(1 2 3) 1 0) OK
(ref #(1 2 3) 1 #\0) OK
(ref "123" 1 #\0) OK
(ref "123" 1 0) OK
(ref "123" 3 #\4) OK
(ref "123" 5 #\4) OK
#t
(factorize '(- (expt x 2) (expt y 2))) OK
(factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2))) OK
(eval (list (list 'lambda '(x y) (factorize '(- (expt x 2) (expt y 2)))) 1 2) (interaction-environment)) OK
#t
```

# Вывод
У меня получилось написать несколько весьма занятных программ, 
однако не без усилия. Я в первые смог на практике применить квазицитирование, 
познакомился с юнит-тестированием и поближе узнал метапрограммирование. 
Также я серьёзно поработал и с системой типов Scheme. Я уверен, что 
приобретённые практические навыки в совокупности с посещением всех лекций 
дадут свои результаты при написании более сложных программ.  
