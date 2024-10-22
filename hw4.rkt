
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; 1. sequence

;; Integer Integer Integer -> ListOfInteger
;; Prodece (listof Integer) from low to high with step of stride, stride is positive
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2. string-append-map
;; ListOfString String -> ListOfString
;; produce a list of strings with a suffix appended to the element of input list of string
(define (string-append-map xs suffix)
  (map (λ (x) (string-append x suffix)) xs))

;; 3. list-nth-mod
;; ListOfX Number -> X
;; (list-nth-mod (list 0 1 2 3 4) 2) -> 2
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (letrec ([f (λ (xs x)
                          (if (= x 0)
                              (car xs)
                              (f (cdr xs) (- x 1))))])
              (f xs (remainder n (length xs))))]))

;; 4. stream-for-n-steps
;; StreamS Number -> LlistOfS
;; produce a list of first n value from s in order
;; Assume n > 0
;(stream-for-n-steps ones 2) (list 1 1)
(define (stream-for-n-steps s n)
  (letrec ([f (λ (x s ans)
                  (if (= x 0)
                      ans
                      (f (- x 1) (cdr (s)) (cons (car (s)) ans))))])
    (reverse (f n s null))))

#;
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
#;
(define nats1
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define ones (lambda () (cons 1 ones)))

;; 5. funny-number-stream
;; produce a stream of natural numbers, numbers divisble by 5 are negated (i.e. 1, 2, 3, 4, -5, 6...)
(define funny-number-stream
  (letrec ([f (λ (x) (cons x (λ () (f (if
                                       (= (remainder (+ x 1) 5) 0)
                                       (- (+ x 1))
                                       (+ (abs x) 1))))))])
    (λ () (f 1))))

;; 6. dan-then-dog
;; produce a stream
;; (stream-for-n-steps dan-then-dog 1) -> (list "dan.jpg")
;; (stream-for-n-steps dan-then-dog 2) -> (list "dan.jpg" "dog.jpg")
;; (stream-for-n-steps dan-then-dog 3) -> (list "dan.jpg" "dog.jpg" "dan.jpg")
(define dan-then-dog
  (letrec ([f (λ (x) (cons x (λ () (f (if (equal? x "dan.jpg")
                                          "dog.jpg"
                                          "dan.jpg")))))])
    (λ () (f "dan.jpg"))))

;; 7. stream-add-zero
;; Stream -> Stream
;; (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1))
;; (stream-for-n-steps (stream-add-zero ones) 2) (list (cons 0 1) (cons 0 1))
(define (stream-add-zero s)
  (letrec ([f (λ (x) (cons (cons 0 (car (x))) (λ () (f (cdr (x))))))])
    (λ () (f s))))

;; 8. cycle-lists
;; ListOfX ListOfY -> Stream
;; produce a stream, combine an element from xs and an element from ys
;; (stream-for-n-steps (cycle-lists [list 1] [list 2]) 1) -> (list (cons 1 2))
;; (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) -> (list (cons 1 "a") (cons 2 "b") (cons 3 "a"))
;; Aussume xs and ys both not empty

(define (cycle-lists xs ys)
  (letrec ([as xs]
           [bs ys]
           [f (λ (xs ys)
                (cons (cons (car xs) (car ys))
                      (λ () (f (if (null? (cdr xs)) as (cdr xs))
                               (if (null? (cdr ys)) bs (cdr ys))))))])
    (λ () (f xs ys))))

;; 9. vector-assoc
;; value Vector -> Boolean/Pair
;; produce the first pair if vector element is a pair and the (car pair) is equal to v
;; (vector-assoc "a" (vector (cons 1 2) (cons "a" 2) (cons "a" 8))) -> (cons "a" 2)
;; (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) -> (cons 4 1)
(define (vector-assoc v vec)
  (letrec ([f (λ (x y)
                (cond [(= (vector-length y) 0) #f]
                      [(and (pair? (vector-ref y 0))
                            (equal? v (car (vector-ref y 0))))
                       (vector-ref y 0)]
                      [else (f x (vector-drop y 1))]))])
    (f v vec)))

;; 10. cached-assoc
;; ListOfX Number -> Function
;; produce a function like assoc
;; ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) -> (cons 3 4)

(define (cached-assoc xs n)
  (letrec ([vec (make-vector n #f)]
           [f (λ (v)
                (if (vector-assoc v vec)
                    (vector-assoc v vec)
                    (begin (vector-set! vec
                                        (if (>= (vector-length vec) n) 0 (vector-length vec))
                                        (assoc v xs))
                           (print vec)
                           (assoc v xs))))])
    (λ (v) (f v))))

;; 11. whiel-less-do
(define-syntax while-less
  (syntax-rules (while-less do)
    [(while-less e1 do e2)
     (letrec ([f (λ (x y)
                  (if (< (y) x)
                      (f x y)
                      #t))])
       (f e1 (λ () e2)))]))
      