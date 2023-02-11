% Лабораторная работа № 5. Интерпретатор конкатенативного языка программирования
% 19 ноября 2022 г.
% Черников Артём, Волохов Александр ИУ9-12Б


# Цель работы 
Написать свой собственный интерпретатор стекового языка программирования. 
На первый взгляд, задача звучит очень трудновыполнимой. 
Написать свой интерпретатор! Но нет ничего невозможного, и я в команде 
с Сашей Волоховым попробую взяться за это задание. 
Что получится? Посмотрим, впереди много работы. 


# Реализация
```scheme
(define ie (interaction-environment)) ;; среда взаимодействия

(define (interpret program init-stack)
  ; базовые операции
  (define operations '(+ - * /)) 
  (define comparisons '(= > <)) 

  ; поиск индекса слова
  (define (word-index word program index) 
  (if (< index (vector-length program))
      (if (equal? (vector-ref program index) word)
          index
          (word-index word program (+ index 1)))
      #f))

  ; проверка на наличие 
  (define (is-in? xs x) 
  (and (not (null? xs)) (or (equal? x (car xs)) (is-in? (cdr xs) x))))

  ; математика и операции сравнения
  (define (math-action action stack) 
    (cons (eval (list action (cadr stack) (car stack)) ie)
          (cddr stack)))
  (define (comparison-action action stack) 
    (cons (if (eval (list action (cadr stack) (car stack)) ie) -1 0)
          (cddr stack)))
  
  (let interpreter ((index 0) (stack init-stack) (return-stack '()) (definitions '()))
    (if (= (vector-length program) index)
        stack
        (let ((word (vector-ref program index)))
          (cond
            ((number? word) (interpreter (+ index 1) (cons word stack)
                                         return-stack definitions))
            ((is-in? operations word) (interpreter (+ index 1) (math-action word stack)
                                                   return-stack definitions))
            ((is-in? comparisons word) (interpreter (+ index 1)
                                                    (comparison-action word stack)
                                                    return-stack definitions))
            ((equal? word 'mod) (interpreter (+ index 1) (cons (remainder
                                                                (cadr stack)
                                                                (car stack))
                                                               (cddr stack))
                                             return-stack definitions))
            ((equal? word 'neg) (interpreter (+ index 1) (cons (- (car stack))
                                                               (cdr stack))
                                             return-stack definitions))
            
            ((equal? word 'not) (interpreter (+ index 1) (cons
                                                          (if (= (car stack) -1) 0 -1)
                                                          (cdr stack))
                                             return-stack definitions))
            ((equal? word 'and) (interpreter (+ index 1) (cons (if (and
                                                                    (= (car stack) -1)
                                                                    (= (cdr stack) -1))
                                                                   -1 0)
                                                               (cddr stack))
                                             return-stack definitions))
            ((equal? word 'or) (interpreter (+ index 1) (cons (if (or (= (car stack) -1)
                                                                      (= (cdr stack) -1)) -1 0)
                                                              (cddr stack))
                                            return-stack definitions))
            
            ((equal? word 'drop) (interpreter (+ index 1) (cdr stack)
                                              return-stack definitions))
            ((equal? word 'swap) (interpreter (+ index 1) (append (list (cadr stack)
                                                                        (car stack))
                                                                  (cddr stack))
                                              return-stack definitions))
            ((equal? word 'dup) (interpreter (+ index 1) (cons (car stack) stack)
                                             return-stack definitions))
            ((equal? word 'over) (interpreter (+ index 1) (cons (cadr stack) stack)
                                              return-stack definitions))
            ((equal? word 'rot) (interpreter (+ index 1) (append (list (caddr stack)
                                                                       (cadr stack)
                                                                       (car stack))
                                                                 (cdddr stack))
                                             return-stack definitions))
            ((equal? word 'depth) (interpreter (+ index 1) (cons (length stack) stack)
                                               return-stack definitions))
            
            ((equal? word 'define) (interpreter (+ (word-index 'end program index) 1)
                                                stack return-stack
                                                (cons (list (vector-ref program (+ index 1))
                                                            (+ index 2)) definitions)))
            ((is-in? '(exit end) word) (interpreter (car return-stack) stack
                                                    (cdr return-stack)
                                                    definitions))
            ((equal? word 'if) (interpreter (if (zero? (car stack))
                                                (+ (word-index 'endif program index) 1)
                                                (+ index 1))
                                            (cdr stack) return-stack definitions))
            ((equal? word 'endif) (interpreter (+ index 1) stack return-stack definitions))
            (else (interpreter (cadr (assoc word definitions)) stack
                               (cons (+ index 1) return-stack) definitions)))))))

# Тестирование
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (interpret #(   define abs
                  dup 0 <
                  if neg endif
                end
                abs    ) ; программа
           '(-9))
(9)
> (interpret #(2 3 * 4 5 * +) '())
(26)
> (interpret #(   define =0? dup 0 = end
                define <0? dup 0 < end
                define signum
                    =0? if exit endif
                    <0? if drop -1 exit endif
                    drop
                    1
                end
                 0 signum
                -5 signum
                10 signum       ) (quote ()))
(1 -1 0)
> (interpret #(   define -- 1 - end
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
                4 factorial     ) (quote ()))
(24 6 2 1 1)
> (interpret #(   define =0? dup 0 = end
                define =1? dup 1 = end
                define -- 1 - end
                define fib
                    =0? if drop 0 exit endif
                    =1? if drop 1 exit endif
                    -- dup
                    -- fib
                    swap fib
                    +
                end
                define make-fib
                    dup 0 < if drop exit endif
                    dup fib
                    swap --
                    make-fib
                end
                10 make-fib     ) (quote ()))
(0 1 1 2 3 5 8 13 21 34 55)


# Выводы
Вот и успешно закончили написание своего собственного интерпретатора. 
На самом деле, потребовалось не так много времени, как мы с Сашей думали. 
Если внимательно прочитать техническое задание, то всё становится понятным. 
В всём коде мы использовали максимально простые и быстрые приёмы, однако, 
можно было бы и усложнить программу, сделать её более изящной и быстрой с 
помощью написания макросов. Но мы так не стали делать, решив оставить наш 
рабочий код в виде, представленным сверху. 
Уже стали заметны серьёзные улучшения в моём владении языком - как будто 
только вчера еле писал рекурсивную программу power для вычисления 
степени числа, а теперь я создал на Scheme свой собственный интерпретатор!
