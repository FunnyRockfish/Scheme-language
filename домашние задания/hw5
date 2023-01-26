'feature-if-else
(define ie (interaction-environment))
(define signs '(+ - * / mod)) ;car + ; cadr - ; caddr * ; cadddr / ; cddddr mod
(define logic '(= > <)) ;car = ; cadr > ; caddr <

(define (member? xs x) ; проверка на принадлежность 
  (and (not (null? xs)) (or (equal? x (car xs)) (member? (cdr xs) x))))

(define (word-index word program index) ; поиск индекса слова
  (if (< index (vector-length program))
      (if (equal? (vector-ref program index) word)
          index
          (word-index word program (+ index 1)))
      #f))

(define (action-execute aliases action stack) ; (5 6 +) -> (11)
  (let ((aliased-action (assoc action aliases)))
    (if aliased-action
        (let ((aliased-action (cadr aliased-action)))
          (eval (list (car aliased-action)
                      (list (cadr aliased-action) (list 'quote stack))
                      (list (caddr aliased-action) (list 'quote stack))) ie))
        (eval (list action (cadr stack) (car stack)) ie))))

(define (math_act action stack) ; математические вычисления
  (define aliases (list (list 'mod '(remainder cadr car)) (list '/ '(quotient cadr car))))
  (cons (action-execute aliases action stack) (cddr stack)))

(define (logic_act action stack) ; логические сравнения
  (cons (if (action-execute '() action stack) -1 0) (cddr stack)))

(define (interpret program init-stack)
  (let interpreter ((index 0) (stack init-stack) (return-stack '()) (definitions '()))
    (if (= (vector-length program) index)
        stack
        (let ((word (vector-ref program index)))
          (cond
            ((number? word) (interpreter (+ index 1) (cons word stack) return-stack definitions))
            ((member? signs word) (interpreter (+ index 1) (math_act word stack) return-stack
                                               definitions))
            ((member? logic word) (interpreter (+ index 1) (logic_act word stack) return-stack
                                               definitions))
            ((equal? word 'not) (interpreter (+ index 1) (cons (if (not (= (car stack) 0)) 0 -1)
                                                               (cdr stack)
                                                               ) return-stack definitions))
            ((equal? word 'neg) (interpreter (+ index 1) (cons (- (car stack)) (cdr stack))
                                             return-stack definitions))
            ((equal? word 'and) (interpreter (+ index 1) (cons (if (or (= (car stack) 0)
                                                                       (= (cadr stack)
                                                                                            0)) 0 -1)
                                                               (cddr stack)) return-stack definitions))
            ((equal? word 'or) (interpreter (+ index 1) (cons (if (and (= (car stack) 0)
                                                                       (= (cadr stack)
                                                                                            0)) 0 -1)
                                                              (cddr stack)) return-stack definitions))
            ((equal? word 'drop) (interpreter (+ index 1) (cdr stack) return-stack definitions))
            ((equal? word 'swap) (interpreter (+ index 1) (append (list (cadr stack) (car stack))
                                                                  (cddr stack))
                                              return-stack definitions))
            ((equal? word 'dup) (interpreter (+ index 1) (cons (car stack) stack) return-stack
                                             definitions))
            ((equal? word 'over) (interpreter (+ index 1) (cons (cadr stack) stack)
                                              return-stack definitions))
            ((equal? word 'rot) (interpreter (+ index 1) (append (list (caddr stack)
                                                                       (cadr stack) (car stack))
                                                                 (cdddr stack))
                                             return-stack definitions))
            ((equal? word 'depth) (interpreter (+ index 1) (cons (length stack) stack) return-stack
                                               definitions))
            ((equal? word 'define) (interpreter (+ (word-index 'end program index) 1)
                                                stack return-stack
                                                (cons (list (vector-ref program (+ index 1))
                                                            (+ index 2)) definitions)))
            ((member? '(exit end) word) (interpreter (car return-stack) stack (cdr return-stack)
                                                     definitions))
            ((equal? word 'if) (if (word-index 'else program index)
                                   (interpreter (if (zero? (car stack)) (+
                                                                         (word-index 'else program index)
                                                                           1)
                                                    (+ index 1)) (cdr stack) return-stack definitions)
                                   (interpreter (if (zero? (car stack)) (+
                                                                         (word-index 'endif program index)
                                                                         1)
                                                    (+ index 1)) (cdr stack) return-stack definitions)
                                   ))
            ((equal? word 'else) (if (zero? (car stack))
                                     (interpreter (+ index 1) (cdr stack) return-stack definitions)
                                     (interpreter (+ (word-index 'endif program index) 1)
                                                  stack return-stack
                                                  definitions)))
            ((equal? word 'endif) (interpreter (+ index 1) stack return-stack definitions))
            ((equal? word 'while) (if (zero? (car stack))
                                      (interpreter (+ (word-index 'wend program index) 1) (cdr stack)
                                                   return-stack definitions)
                                      (interpreter (+ index 1) (cdr stack) (cons index return-stack)
                                                   definitions)))
            ((equal? word 'wend) (interpreter (car return-stack) stack (cdr return-stack) definitions))
            ((equal? word 'repeat) (interpreter (+ index 1) stack (cons index return-stack) definitions))
            ((equal? word 'until) (interpreter (if (zero? (car stack)) (car return-stack) (+ index 1))
                                               (cdr stack) (cdr return-stack) definitions))
            (else (interpreter (cadr (assoc word definitions)) stack (cons (+ index 1)
                                                                           return-stack) definitions)))))))
