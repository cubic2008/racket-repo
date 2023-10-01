;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2023-09-cs115-a2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Q1

(define (host-play-date? home friend1 friend2 friend3 max-difference)
  (and (or (and (even? home) (even? friend1) (even? friend2) (even? friend3))
           (and (odd? home) (even? friend1) (even? friend2) (even? friend3)))
       (<= (abs (- home friend1)) max-difference)
       (<= (abs (- home friend2)) max-difference)
       (<= (abs (- home friend3)) max-difference)))

(check-expect (host-play-date? 100 102 108 98 8) #true)
(check-expect (host-play-date? 98 100 102 108 8) #false)
(check-expect (host-play-date? 100 101 102 99 8) #false)

;; Q2

(define (speeding-fine speed limit)
  (+ (* (- speed limit)
        (cond [(< (- speed limit) 10) 0]
              [(< (- speed limit) 20) 3]
              [(< (- speed limit) 30) 4.5]
              [(< (- speed limit) 50) 7]
              [else 9.75]))
     (cond [(or (and (>= (- speed limit) 40) (<= limit 80))
                (>= (- speed limit) 50)
                (>= speed 150)) 10000]
           [else 0])))

(check-expect (speeding-fine 50 60) 0)
(check-expect (speeding-fine 59 50) 0)
(check-expect (speeding-fine 60 50) 30)
(check-expect (speeding-fine 139 110) 130.5)
(check-expect (speeding-fine 100 50) 10487.5)
(check-expect (speeding-fine 125 85) 280)
(check-expect (speeding-fine 150 140) 10030)

;; Q3

(define (sum-of-score d d1 d2 d3 d4 d5)
  (+ (cond [(= d1 d) d1] [else 0])
     (cond [(= d2 d) d2] [else 0])
     (cond [(= d3 d) d3] [else 0])
     (cond [(= d4 d) d4] [else 0])
     (cond [(= d5 d) d5] [else 0])))

(check-expect (sum-of-score 1 5 5 5 5 5) 0)
(check-expect (sum-of-score 2 5 5 5 5 5) 0)
(check-expect (sum-of-score 3 5 5 5 5 5) 0)
(check-expect (sum-of-score 4 5 5 5 5 5) 0)
(check-expect (sum-of-score 5 5 5 5 5 5) 25)
(check-expect (sum-of-score 6 5 5 5 5 5) 0)
(check-expect (sum-of-score 1 3 3 3 3 6) 0)
(check-expect (sum-of-score 2 3 3 3 3 6) 0)
(check-expect (sum-of-score 3 3 3 3 3 6) 12)
(check-expect (sum-of-score 4 3 3 3 3 6) 0)
(check-expect (sum-of-score 5 3 3 3 3 6) 0)
(check-expect (sum-of-score 6 3 3 3 3 6) 6)
(check-expect (sum-of-score 1 2 2 5 5 5) 0)
(check-expect (sum-of-score 2 2 2 5 5 5) 4)
(check-expect (sum-of-score 3 2 2 5 5 5) 0)
(check-expect (sum-of-score 4 2 2 5 5 5) 0)
(check-expect (sum-of-score 5 2 2 5 5 5) 15)
(check-expect (sum-of-score 6 2 2 5 5 5) 0)
(check-expect (sum-of-score 1 2 2 4 4 6) 0)
(check-expect (sum-of-score 2 2 2 4 4 6) 4)
(check-expect (sum-of-score 3 2 2 4 4 6) 0)
(check-expect (sum-of-score 4 2 2 4 4 6) 8)
(check-expect (sum-of-score 5 2 2 4 4 6) 0)
(check-expect (sum-of-score 6 2 2 4 4 6) 6)
(check-expect (sum-of-score 1 1 1 1 3 5) 3)
(check-expect (sum-of-score 2 1 1 1 3 5) 0)
(check-expect (sum-of-score 3 1 1 1 3 5) 3)
(check-expect (sum-of-score 4 1 1 1 3 5) 0)
(check-expect (sum-of-score 5 1 1 1 3 5) 5)
(check-expect (sum-of-score 6 1 1 1 3 5) 0)
(check-expect (sum-of-score 1 1 2 3 4 5) 1)
(check-expect (sum-of-score 2 1 2 3 4 5) 2)
(check-expect (sum-of-score 3 1 2 3 4 5) 3)
(check-expect (sum-of-score 4 1 2 3 4 5) 4)
(check-expect (sum-of-score 5 1 2 3 4 5) 5)
(check-expect (sum-of-score 6 1 2 3 4 5) 0)
(check-expect (sum-of-score 1 2 3 4 5 6) 0)
(check-expect (sum-of-score 2 2 3 4 5 6) 2)
(check-expect (sum-of-score 3 2 3 4 5 6) 3)
(check-expect (sum-of-score 4 2 3 4 5 6) 4)
(check-expect (sum-of-score 5 2 3 4 5 6) 5)
(check-expect (sum-of-score 6 2 3 4 5 6) 6)


(define (yahtzee-score d1 d2 d3 d4 d5)
  (cond [(and (= d1 d2) (= d2 d3) (= d3 d4) (= d4 d5)) 50]
        [else 0]))

(check-expect (yahtzee-score 5 5 5 5 5) 50)
(check-expect (yahtzee-score 3 3 3 3 6) 0)
(check-expect (yahtzee-score 2 2 5 5 5) 0)
(check-expect (yahtzee-score 2 2 4 4 6) 0)
(check-expect (yahtzee-score 1 1 1 3 5) 0)
(check-expect (yahtzee-score 1 2 3 4 5) 0)
(check-expect (yahtzee-score 2 3 4 5 6) 0)


(define (straight-score d1 d2 d3 d4 d5)
  (cond [(and (= (- d2 d1) 1) (= (- d3 d2) 1) (= (- d4 d3) 1) (= (- d5 d4) 1)) 40]
        [else 0]))

(check-expect (straight-score 5 5 5 5 5) 0)
(check-expect (straight-score 3 3 3 3 6) 0)
(check-expect (straight-score 2 2 5 5 5) 0)
(check-expect (straight-score 2 2 4 4 6) 0)
(check-expect (straight-score 1 1 1 3 5) 0)
(check-expect (straight-score 1 2 3 4 5) 40)
(check-expect (straight-score 2 3 4 5 6) 40)

(define (full-house-score d1 d2 d3 d4 d5)
  (cond [(and (= d1 d2) (= d4 d5)
              (or (and (= d2 d3) (not (= d3 d4)))
                  (and (not (= d2 d3)) (= d3 d4)))) 25]
        [else 0]))

(check-expect (full-house-score 5 5 5 5 5) 0)
(check-expect (full-house-score 3 3 3 3 6) 0)
(check-expect (full-house-score 2 2 5 5 5) 25)
(check-expect (full-house-score 2 2 4 4 6) 0)
(check-expect (full-house-score 1 1 1 3 5) 0)
(check-expect (full-house-score 1 2 3 4 5) 0)
(check-expect (full-house-score 2 3 4 5 6) 0)

(define (simplified-yahtzee-score d1 d2 d3 d4 d5)
  (max (sum-of-score 1 d1 d2 d3 d4 d5)
       (sum-of-score 2 d1 d2 d3 d4 d5)
       (sum-of-score 3 d1 d2 d3 d4 d5)
       (sum-of-score 4 d1 d2 d3 d4 d5)
       (sum-of-score 5 d1 d2 d3 d4 d5)
       (sum-of-score 6 d1 d2 d3 d4 d5)
       (yahtzee-score d1 d2 d3 d4 d5)
       (straight-score d1 d2 d3 d4 d5)
       (full-house-score d1 d2 d3 d4 d5)))

; test cases before applying 3 special rules.
;(check-expect (simplified-yahtzee-score 5 5 5 5 5) 25)
;(check-expect (simplified-yahtzee-score 3 3 3 3 6) 12)
;(check-expect (simplified-yahtzee-score 2 2 5 5 5) 15)
;(check-expect (simplified-yahtzee-score 2 2 4 4 6) 8)
;(check-expect (simplified-yahtzee-score 1 1 1 3 5) 5)
;(check-expect (simplified-yahtzee-score 1 2 3 4 5) 5)
;(check-expect (simplified-yahtzee-score 2 3 4 5 6) 6)
(check-expect (simplified-yahtzee-score 5 5 5 5 5) 50)
(check-expect (simplified-yahtzee-score 3 3 3 3 6) 12)
(check-expect (simplified-yahtzee-score 2 2 5 5 5) 25)
(check-expect (simplified-yahtzee-score 2 2 4 4 6) 8)
(check-expect (simplified-yahtzee-score 1 1 1 3 5) 5)
(check-expect (simplified-yahtzee-score 1 2 3 4 5) 40)
(check-expect (simplified-yahtzee-score 2 3 4 5 6) 40)


;(cond [(and (= d1 d2) (= d2 d3) (= d3 d4) (= d4 d5)) (* d1 * 5)]
;        [