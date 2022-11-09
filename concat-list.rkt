;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname concat-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; append an element to a list
(define (appendlst1 lst1 e)
  (cond [(empty? lst1) (cons e empty)]
        [else (cons (first lst1) (appendlst1 (rest lst1) e))]))

(appendlst1 ll1 'D)

;; append lst2 to lst1, by utilizing appendlst1
;; This would be a bad performance O(N^2)
(define (appendlst2 lst1 lst2)
  (cond [(empty? lst2) lst1]
        [else (appendlst2 (appendlst1 lst1 (first lst2)) (rest lst2))]))

(appendlst2 ll1 ll2)

;; Accumulatively append lst2 to lst1
(define (appendlst3/acc lst1 lst2 lst)
  (cond [(empty? lst2) lst]
        [(empty? lst1) (appendlst3/acc lst1 (rest lst2) (cons (first lst2) lst))]
        [else (appendlst3/acc (rest lst1) lst2 (cons (first lst1) lst))]))

(appendlst3/acc ll1 ll2 empty)

;(define (repeat-list lst)
;  (cond [(empty? lst) empty]
;        [else (cons (first lst) (repeat-list (rest lst)))]))


;; Accumulateive reverse a lst
(define (reverse-list/acc lst finallist)
  (cond [(empty? lst) finallist]
        [else (reverse-list/acc (rest lst) (cons (first lst) finallist))]))

;; Wraper of reverse
(define (reverse-list lst)
  (reverse-list/acc lst empty))
  
(reverse-list list1)


;; Wrapper of accumulatively concatenation/apppending lst2 to lst1
;; the intermediate result is a reversed list, so I calls reverst-list
;; to reverse it again.
(define (appendlst3 lst1 lst2)
  (reverse-list (appendlst3/acc ll1 ll2 empty)))

(appendlst3 ll1 ll2)

