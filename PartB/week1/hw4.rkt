
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;4
(define (stream-for-n-steps s n)
  (let ([yield (s)])
    (if (= n 0)
        null
        (cons (car yield) (stream-for-n-steps (cdr yield) (- n 1))))))

;5
(define funny-number-stream
  (letrec ([f (lambda (n) (cons (if (= (remainder n 5) 0)
                                    (- 0 n)
                                    n)
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 1))))

;6
(define dan-then-dog
  (letrec ([f1 (lambda () (cons "dan.jpg" (lambda () (f2))))]
           [f2 (lambda () (cons "dog.jpg" (lambda () (f1))))])
    f1))

;7
(define (stream-add-zero s)
  (lambda ()
    (let ([yield (s)])
      (cons (cons 0 (car yield)) (stream-add-zero (cdr yield))))))

;8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (if (< n (vector-length vec))
                                  (let ([nth (vector-ref vec n)])
                                    (if (and (pair? nth) (equal? (car nth) v))
                                        nth
                                        (f (+ n 1))))
                                  #f))])
    (f 0)))

;10
(define (cached-assoc xs n)
  (let([cache (make-vector n #f)]
       [place-ref 0])
    (lambda (v) (let ([cache-res (vector-assoc v cache)])
                  (if cache-res
                      cache-res
                      (let ([assoc-res (assoc v xs)])
                        (if assoc-res
                            (begin (vector-set! cache place-ref assoc-res)
                                   (if (= place-ref (- n 1))
                                       (set! place-ref 0)
                                       (set! place-ref (+ place-ref 1)))
                                   assoc-res)
                            #f)))))))

;11 (challange problem)
(define-syntax while-less
  (syntax-rules (do)
     [(while-less e1 do e2)
      (let ([x e1])
        (letrec ([loop (lambda () (let ([y e2])
                                    (if (or (not (number? y)) (<= x y))
                                      #t
                                      (loop))))])
          (loop)))]))
        
          
                                     
                                       
    
