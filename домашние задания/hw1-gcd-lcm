(define (my-gcd a b)
  (if (= b 0)
      a
      (modul(my-gcd b (remainder a b)))))
          
(define (modul a)
  (if (< a 0)
      (- a (* 2 a))
      (+ a 0)))

(define (my-lcm a b)
  (if (not(=(gcd a b) 0))
      (/ (modul (* a b))(my-gcd a b))))


(define (prime? n)
  (define count 2)
  (if ( and (= (remainder n count) 0) ( > n count))
      #f)
  (if ( and (> (remainder n count) 0) ( > n count))
      (set! count (+ count 1))
      count)) 





;;(define (prime? a count)
  ;;(if (> a count)
      ;;(if (> a 0) 
         ;; (if (=(remainder a count) 0)
             ;; #f
              ;;(prime? a count + 1)))))
(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))
