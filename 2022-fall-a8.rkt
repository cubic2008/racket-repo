;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2022-fall-a8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Q1a
(define (my-ormap pred? lst)
  (cond [(empty? lst) false]
        [else (or (pred? (first lst)) (my-ormap pred? (rest lst)))]))


(check-expect (my-ormap zero? '(8 6 7 5 3 0 9)) true)
(check-expect (my-ormap even? '(8 6 7 5 3 0 9)) true)
(check-expect (my-ormap odd? '(8 6 7 5 3 0 9)) true)
(check-expect (my-ormap zero? '(8 6 7 5 3 10 9)) false)
(check-expect (my-ormap even? '(1 11 7 5 3 1 9)) false)
(check-expect (my-ormap odd? '(8 6 6 52 32 0 96)) false)
(check-expect (my-ormap (lambda (x) (> x 10)) '(8 6 7 5 3 0 9)) false)
(check-expect (my-ormap (lambda (x) (> x 6)) '(8 6 7 5 3 0 9)) true)

;; Q1b
(define (pred?-ormap val pred?-list)
  (cond [(empty? pred?-list) false]
        [else (or ((first pred?-list) val) (pred?-ormap val (rest pred?-list)))]))

(check-expect (pred?-ormap 5 (list zero? even? negative? posn? inexact?)) false)
(check-expect (pred?-ormap 2 (list zero? even? negative? posn? inexact?)) true)
(check-expect (pred?-ormap -5 (list zero? even? negative? posn? inexact?)) true)
(check-expect (pred?-ormap 5 (list zero? even?(lambda (x) (> x 10)) (lambda (x) (< x 3)))) false)
(check-expect (pred?-ormap 15 (list zero? even?(lambda (x) (> x 10)) (lambda (x) (< x 3)))) true)
(check-expect (pred?-ormap 1 (list zero? even?(lambda (x) (> x 10)) (lambda (x) (< x 3)))) true)

;; Q2b
(define (nested-count lox)
  (cond [(empty? lox) 0]
        [(cons? (first lox)) (+ (nested-count (first lox)) (nested-count (rest lox)))]
        [else (add1 (nested-count (rest lox)))]))

(define lox1 '(A1 A2 (B1 B2 (C1 C2 C3) B3 (D1 D2 D3 D4 D5 D6) B4 B5) A3 (E1 E2 (F1 F2 F3) E3) A4 A5 (G1 G2 G3)))
(check-expect (nested-count lox1) 28)

;; Q2c
(define (nested-sum lon)
  (cond [(empty? lon) 0]
        [(cons? (first lon)) (+ (nested-sum (first lon)) (nested-sum (rest lon)))]
        [else (+ (first lon) (nested-sum (rest lon)))]))

(define lon1 '(1 2 (11 12 (21 22 23) 13 (31 32 33 34 35 36) 14 15) 3 (41 42 (51 52 53) 43) 4 5 (61 62 63) 6))
(check-expect (nested-count lon1) 29)
(check-expect (nested-sum lon1) 821)

;; Q2d
(define (nested-member? val lox)
  (cond [(empty? lox) false]
        [(cons? (first lox)) (or (nested-member? val (first lox)) (nested-member? val (rest lox)))]
        [(equal? (first lox) val) true]
        [else (nested-member? val (rest lox))]))

(check-expect (nested-member? 'A1 lox1) true)
(check-expect (nested-member? 'A3 lox1) true)
(check-expect (nested-member? 'A5 lox1) true)
(check-expect (nested-member? 'B2 lox1) true)
(check-expect (nested-member? 'C3 lox1) true)
(check-expect (nested-member? 'D4 lox1) true)
(check-expect (nested-member? 'E1 lox1) true)
(check-expect (nested-member? 'F2 lox1) true)
(check-expect (nested-member? 'HH lox1) false)
(check-expect (nested-member? 1 lon1) true)
(check-expect (nested-member? 2 lon1) true)
(check-expect (nested-member? 11 lon1) true)
(check-expect (nested-member? 12 lon1) true)
(check-expect (nested-member? 15 lon1) true)
(check-expect (nested-member? 21 lon1) true)
(check-expect (nested-member? 63 lon1) true)
(check-expect (nested-member? 6 lon1) true)
(check-expect (nested-member? 100 lon1) false)
(check-expect (nested-member? 'hot-dog '((pizza) (hamburger) (((hot-dog))))) true)
(check-expect (nested-member? 'bread '((pizza) (hamburger) (((hot-dog))))) false)

;; Q2e
(define (nested-ref loxx kk)
  (local [(define (nested-ref/helper lox k n)
            (cond [(empty? lox) (list empty n)]
                  [(cons? (first lox))
                   (local [(define ref-in-list (nested-ref/helper (first lox) k n))]
                     (cond [(empty? (first ref-in-list)) (nested-ref/helper (rest lox) (- kk (second ref-in-list)) (second ref-in-list))]
                           [else ref-in-list]))]
                  [(= k 0) (list (first lox) n)]
                  [else (nested-ref/helper (rest lox) (sub1 k) (add1 n))]))]
;     (nested-ref/helper loxx kk 0)))
    (first (nested-ref/helper loxx kk 0))))

(define lon2 '(1 2 (11 12 (21 22 23) )))
(define lon3 '(1 (2) 3))

(check-expect (nested-ref lon1 0) 1)
(check-expect (nested-ref lon1 1) 2)
(check-expect (nested-ref lon1 2) 11)
(check-expect (nested-ref lon1 3) 12)
(check-expect (nested-ref lon1 4) 21)
(check-expect (nested-ref lon1 5) 22)
(check-expect (nested-ref lon1 6) 23)
(check-expect (nested-ref lon1 7) 13)
(check-expect (nested-ref lon1 8) 31)
(check-expect (nested-ref lon1 9) 32)
(check-expect (nested-ref lon1 10) 33)
(check-expect (nested-ref lon1 11) 34)
(check-expect (nested-ref lon1 12) 35)
(check-expect (nested-ref lon1 13) 36)
(check-expect (nested-ref lon1 14) 14)
(check-expect (nested-ref lon1 15) 15)
(check-expect (nested-ref lon1 16) 3)
(check-expect (nested-ref lon1 17) 41)
(check-expect (nested-ref lon1 18) 42)
(check-expect (nested-ref lon1 19) 51)
(check-expect (nested-ref lon1 20) 52)
(check-expect (nested-ref lon1 21) 53)
(check-expect (nested-ref lon1 22) 43)
(check-expect (nested-ref lon1 23) 4)
(check-expect (nested-ref lon1 24) 5)
(check-expect (nested-ref lon1 25) 61)
(check-expect (nested-ref lon1 26) 62)
(check-expect (nested-ref lon1 27) 63)
(check-expect (nested-ref lon1 28) 6)
(check-expect (nested-ref lon1 29) empty)

;; Q2f
(define (nested-filter pred? lox)
  (cond [(empty? lox) empty]
        [(cons? (first lox)) (cons (nested-filter pred? (first lox)) (nested-filter pred? (rest lox)))]
        [(pred? (first lox)) (cons (first lox) (nested-filter pred? (rest lox)))]
        [else (nested-filter pred? (rest lox))]))

(check-expect (nested-filter (lambda (x) (> x 10)) lon1)
                (list (list 11 12 (list 21 22 23) 13 (list 31 32 33 34 35 36) 14 15) (list 41 42 (list 51 52 53) 43) (list 61 62 63)))
(check-expect (nested-filter (lambda (x) (> x 20)) lon1)
                (list (list (list 21 22 23) (list 31 32 33 34 35 36)) (list 41 42 (list 51 52 53) 43) (list 61 62 63)))
(check-expect (nested-filter (lambda (x) (> x 32)) lon1)
                (list (list (list ) (list 33 34 35 36)) (list 41 42 (list 51 52 53) 43) (list 61 62 63)))
(check-expect (nested-filter (lambda (x) (> x 62)) lon1)
                (list (list (list ) (list )) (list (list ) ) (list 63)))
(check-expect (nested-filter (lambda (x) (> x 100)) lon1)
                (list (list (list ) (list )) (list (list ) ) (list)))
(check-expect (nested-filter (lambda (x) true) lon1) lon1)

;; Q2g
;(define (nested-cleanup lox)
;  (cond [(empty? lox) empty]
;        [(cons? (first lox))
;         (local [(define sub-list (nested-cleanup (rest lox)))]
;           (cond [(empty? sub-list) (cons 'XXX (nested-cleanup (rest lox)))]
;                 [else (cons sub-list (nested-cleanup (rest lox)))]))]
;        [else (cons (first lox) (nested-cleanup (rest lox)))]))

(define (nested-cleanup lox)
  (local [(define (nested-cleanup/helper lox)
            (cond [(empty? lox) empty]
                  [(empty? (first lox)) (nested-cleanup/helper (rest lox))]
                  [(cons? (first lox))
                   (local [(define sub-list (nested-cleanup/helper (first lox)))]
                     (cond [(empty? sub-list) (nested-cleanup/helper (rest lox))]
                           [else (cons sub-list (nested-cleanup/helper (rest lox)))]))]
                  [else (cons (first lox) (nested-cleanup/helper (rest lox)))]))
          (define cleaned-list (nested-cleanup/helper lox))]
    (cond [(empty? cleaned-list) false]
          [else cleaned-list])))

(check-expect (nested-cleanup (list (list '() '()) (list '()) (list 63))) (list (list 63)))
(check-expect (nested-cleanup (list 1 (list))) (list 1))
(check-expect (nested-cleanup (nested-filter (lambda (x) (> x 10)) lon1))
              (list (list 11 12 (list 21 22 23) 13 (list 31 32 33 34 35 36) 14 15) (list 41 42 (list 51 52 53) 43) (list 61 62 63)))
;(nested-filter (lambda (x) (> x 62)) lon1)
(check-expect (nested-cleanup (nested-filter (lambda (x) (> x 62)) lon1)) (list (list 63)))
;(nested-filter (lambda (x) (> x 100)) lon1)
(check-expect (nested-cleanup (nested-filter (lambda (x) (> x 100)) lon1)) false)
(check-expect (nested-cleanup '(1 () 2 () () 3)) '(1 2 3))
(check-expect (nested-cleanup '(1 (()()) 2 ((3 () (()))) )) '(1 2 ((3))))
(check-expect (nested-cleanup '(()(()())(())())) false)

;; Q3a

;; ---------- A4Q5 ----------
(define (fizz-buzz start end fizz buzz)
  (cond [(< end start) empty]
        [(= (remainder start fizz) 0)
         (cond [(= (remainder start buzz) 0)(cons 'honk (fizz-buzz (add1 start) end fizz buzz))]
               [else (cons 'fizz (fizz-buzz (add1 start) end fizz buzz))])]
        [(= (remainder start buzz) 0) (cons 'buzz (fizz-buzz (add1 start) end fizz buzz))]
        [else (cons start (fizz-buzz (add1 start) end fizz buzz))]))

(check-expect (fizz-buzz 8 15 3 5) (list 8 'fizz 'buzz 11 'fizz 13 14 'honk))

;; --------------------------
(define (make-div-pred n)
  (local [(define (div? m) (= (remainder m n) 0))]
    div?))

(define div3? (make-div-pred 3))
(define div5? (make-div-pred 5))

(check-expect ((make-div-pred 3) 27) true)
(check-expect ((make-div-pred 3) 10) false)
(check-expect (div5? 25) true)
(check-expect (div5? 32) false)

;; Q3b
(define (fizz-buzz-2 start end pred?-list)
  (local [(define (replace-map val pred?-list)
            (cond [(empty? pred?-list) val]
                  [((second (first pred?-list)) val) (first (first pred?-list))]
                  [else (replace-map val (rest pred?-list))]))]
    (cond [(< end start) empty]
          [else (cons (replace-map start pred?-list) (fizz-buzz-2 (add1 start) end pred?-list))])))

(check-expect (fizz-buzz-2 8 15 (list (list 'honk (make-div-pred 15))
                                      (list 'fizz (make-div-pred 3))
                                      (list 'buzz (make-div-pred 5))))
              '(8 fizz buzz 11 fizz 13 14 honk))
(check-expect (fizz-buzz-2 -3 3 (list (list "donut" zero?)
                                      (list "even" even?)
                                      (list 'neg negative?)))
              '(neg "even" neg "donut" 1 "even" 3))
(check-expect (fizz-buzz-2 10 9 (list (list "donut" zero?)
                                      (list "even" even?)
                                      (list 'neg negative?)))
              '())
(check-expect (fizz-buzz-2 10 10 (list (list "donut" zero?)
                                      (list "even" even?)
                                      (list 'neg negative?)))
              '("even"))

;; Q4a
(require "heap-support.rkt")
(define example-heap (make-hnode 1
                                 (make-hnode 15
                                             (make-hnode 60
                                                         (make-hnode 70 '() '())
                                                         '())
                                             (make-hnode 20
                                                         (make-hnode 40 '() '())
                                                         '()))
                                 (make-hnode 5
                                             (make-hnode 10
                                                         (make-hnode 50 '() '())
                                                         '())
                                             (make-hnode 30 '() '()))))

(heap-print example-heap number->string)
