% Лабораторная работа № 2. Рекурсия, процедуры высшего порядка, обработка списков
% 23 сентября 2022 г.
% Черников Артём, ИУ9-12Б
# Цель работы
Пополнить свою, пока ещё небольшую копилку знаний в языке Scheme, 
такими полезными и самыми важными в функциональных языках программирования
вещами, как: рекурсия, процедуры высшего порядка и обработка списков. 
Последнее особо важно, ведь LISP - "старший брат" Scheme - значит List Prozessing, 
а значит списки лучше всего понять уже во время начала учёбы, а не в конце:)

# Реализация
; 1
(define (count el xs)
  (if (not (null? xs))
      (if (equal? (car xs) el)
          (+ 1 (count el (cdr xs)))
          (count el (cdr xs))
          )
      0
      ))

;; 2
(define (delete pred? xs)
  (define (loop pred? xs xs-ans)
    (if (null? xs)
        xs-ans
        (if (pred? (car xs))
            (loop pred? (cdr xs) xs-ans)
            (loop pred? (cdr xs) (append xs-ans (list (car xs)))))))
  (loop pred? xs '()))
 
;; 3
(define (iterate f x n)
  (define (loop f xs n)
    (if (= n 0)
        xs
        (loop f (cons x (map f xs)) (- n 1))))
  (if (= n 0) '() (loop f (list x) (- n 1))))
  
;;4
(define (intersperse e xs)
  (if (not (null? xs))
      (if (not (null? (cdr xs)))
          (append (list (car xs)) (list e) (intersperse e (cdr xs)))
          xs)(list)))

;;5
(define (any? pred? xs)
  (if (not (null? xs))
      (or (pred? (car xs)) (any? pred? (cdr xs)))
      #f))

(define (all? pred? xs)
  (if (not (null? xs))
      (and (pred? (car xs)) (any? pred? (cdr xs)))
      #t))

;;6
(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))
 
(define (o . flist)
  (define (applist flist x)
    (cond ((null? flist) x)
          (#t ((car flist) (applist (cdr flist) x)))))
  (lambda (x) (applist flist x)))     
 
(display ((o f g h) 1))
(newline)
(display ((o f g) 1))
(newline)
(display ((o h) 1))
(newline)
(display ((o)  1))

# Тестирование
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
-1
5
-1
1
> (count 'a '(a b c a))
2
> (delete even? '(0 1 2 3))
(1 3)
> (iterate (lambda (x) (* 2 x)) 1 6)
(1 2 4 8 16 32)
> (intersperse 'x '(1 2 3 4))
(1 x 2 x 3 x 4)
> (any? odd? '(1 3 5 7))
#t

# Вывод
Я научился производить довольно занятные действия со списками, 
применяя уже достаточно мощные инструменты, или, 
если можно так сказать, "хлеб с маслом" настоящего "Лиспера" - рекурсия 
и обработка списков. Хоть мне ещё и далеко до написания серьёзных программ, 
но первый шаг к этому уже сделан. 
