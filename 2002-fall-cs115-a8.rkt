;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2002-fall-cs115-a8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Q1

;; Q2
(define (most-profit lon)
  (local [(define (most-profit/acc low lon n)
            (cond [(empty? lon) n]
                  [(> (- (first lon) low) n) (most-profit/acc low (rest lon) (- (first lon) low))]
                  [else (most-profit/acc low (rest lon) n)]))]
    (cond [(empty? lon) 0]
          [else (local [(define curr-profit (most-profit/acc (first lon) (rest lon) 0))
                        (define rest-profit (most-profit (rest lon)))]
                  (cond [(< curr-profit rest-profit) rest-profit]
                        [else curr-profit]))])))


(define lst1 (list 4 7 14 1 8 12 7 0 6))
(define lst2 (list 5 7 8 11 9 1 4 3 4 2 5))

;(most-profit/acc 4 (rest lst1) 0)
(most-profit lst1)
(most-profit lst2)

;; Q3
(define (trace matrix)
  (cond [(empty? matrix) empty]
        [else (cons (first (first matrix)) (trace (map rest (rest matrix))))]))

(define m1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(trace (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))


