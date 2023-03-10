% Лабораторная работа № 1. Основы ЯП Scheme и среды разработки DrRacket
% 9 сентября 2022 г.
% Черников Артём, ИУ9-12Б


# Цель работы
Познать основы такого мощного функционального языка программирования,
одного из диалектов LISPa - Scheme, созданного в конце 70-х годов прошлого века
Гаем Стилом и Джеральдом Сассменом. 


# Реализация
```scheme
(define(! n)
  (if (> n 0)
      (* (! (- n 1)) n)
      1))
(define(my-* x y)
  (if (> y 0)
      (+ (my-* x (- y 1)) x)
      0))


(define(my-abs x)
  (if (< x 0)
      (* x -1)
      x))

(define true #t)
(define false #f)

(define (my-odd? x)
 ( if (>(remainder x 2)0) 
  true
  false))

(define (my-even? x)
 ( if (=(remainder x 2)0) 
  true
  false))

(define(power b e)
  (expt b e))


# Тестирование 
Welcome to DrRacket, version 8.6 [cs].
Language: R5RS; memory limit: 128 MB.
> (! 6)
720
> (my-abs -100)
100
> (my-odd? 100)
#f
> (power 5 5)
3125

# Вывод
Я научился создавать уже какие-то микропрограммы,
которые будут мне полезны в дальнейшем написании различного рода кода на этом
интересном языке. Увидел некоторые преимущества функционального подхода,
однако на этот счёт ведутся долгие споры, какой же подход всё-таки лучше. 
По-моему мнению, пока вывод делать рано, какой мне лично подход ближе, 
но могу сказать точно - Scmeme очень интересный и необычный для меня язык,
который поможет "поставить" мне компьютерное, если можно так сказать, понимание.
