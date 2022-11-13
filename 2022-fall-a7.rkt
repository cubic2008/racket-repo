;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2022-fall-a7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(define-struct student(name age))

;(define s1 (make-student "John" 21))

;(build-list 10 (lambda (x) (* 2 x)))

;;; A7

;; Q1b
(define-struct anode(name parent1 parent2))
(define copyright-free-ancestors 
  (make-anode"Liso"
             (make-anode "Homern't"
                         (make-anode "Abrahum"
                                    (make-anode "Arville" empty empty)
                                    empty)
                         (make-anode "Mana" empty empty))
             (make-anode "Merge"
                        (make-anode "Cloncy"
                                   empty
                                   (make-anode "Bombi" empty empty))
                        empty)
  )
)
                
(define (find-subtree AT name)
    (cond[(empty? AT) empty]
         [(string=? name (anode-name AT)) AT]
         [else
           (local [(define AT1 (find-subtree (anode-parent1 AT) name))
                   (define AT2 (find-subtree (anode-parent2 AT) name))]
;             AT2)]))
             (cond[(empty? AT1) AT2]
                  [else AT1]))]))

(check-expect (find-subtree copyright-free-ancestors "Liso") copyright-free-ancestors)
(check-expect (find-subtree copyright-free-ancestors "Homern't") (anode-parent1 copyright-free-ancestors))
(check-expect (find-subtree copyright-free-ancestors "Merge") (anode-parent2 copyright-free-ancestors))
(check-expect (find-subtree copyright-free-ancestors "Abrahum") (anode-parent1 (anode-parent1 copyright-free-ancestors)))
(check-expect (find-subtree copyright-free-ancestors "Mana") (anode-parent2 (anode-parent1 copyright-free-ancestors)))
(check-expect (find-subtree copyright-free-ancestors "Arville") (anode-parent1 (anode-parent1 (anode-parent1 copyright-free-ancestors))))
(check-expect (find-subtree copyright-free-ancestors "Cloncy") (anode-parent1 (anode-parent2 copyright-free-ancestors)))
(check-expect (find-subtree copyright-free-ancestors "Bombi") (anode-parent2 (anode-parent1 (anode-parent2 copyright-free-ancestors))))
;(find-subtree copyright-free-ancestors "Cloncy")

;; Q1c

;(define (get-f-generation AT n)
;  (local [(define (get-f-generation/acc ATs m lon)
;            (cond [(empty? ATs) empty]
;                  [(= m 0) (cons (anode-name ATs) empty)]
;                  [else
;                   (local [(define llon (get-f-generation/acc (anode-parent1 ATs) (- m 1) empty))
;                           (define rlon (get-f-generation/acc (anode-parent2 ATs) (- m 1) empty))]
;                     (append llon rlon))]))]
;    (get-f-generation/acc AT n empty)))

(define (get-f-generation AT n)
  (cond [(empty? AT) empty]
        [(= n 0) (cons (anode-name AT) empty)]
        [else
         (local [(define llon (get-f-generation (anode-parent1 AT) (- n 1)))
                 (define rlon (get-f-generation (anode-parent2 AT) (- n 1)))]
           (append llon rlon))]))

  
(check-expect (get-f-generation copyright-free-ancestors 0) '("Liso"))
(check-expect (get-f-generation copyright-free-ancestors 1) '("Homern't" "Merge"))
(check-expect (get-f-generation copyright-free-ancestors 2) '("Abrahum" "Mana" "Cloncy"))
(check-expect (get-f-generation copyright-free-ancestors 3) '("Arville" "Bombi"))
(check-expect (get-f-generation copyright-free-ancestors 4)' ())

;; Q1d
#|
(define (get-f-descendants-path AT name)
  (local [(define get-f-descendants-path/acc AT name lon)
    (cond [(empty? AT) lon]
          [(string=? name (anode-name AT)) (cons (anode-name AT) empty)]
          [else (local [(define parent1 (get-f-descendants-path (anode-parent1 AT) name lon))
                        (define parent2 (get-f-descendants-path (anode-parent2 AT) name lon))]
                  (cond [(and (empty? parent1) (empty? parent2)) empty]
                        [(empty? parent1) (cons (anode-name AT) parent2)]
                        [else (cons (anode-name AT) parent1)]))])]
    (get-f-descendants-path/acc AT name empty)))
|#


;(define (get-f-descendants-path AT name)
;  (cond [(empty? AT) empty]
;        [(string=? name (anode-name AT)) (cons (anode-name AT) empty)]
;        [else (local [(define parent1 (get-f-descendants-path (anode-parent1 AT) name))
;                      (define parent2 (get-f-descendants-path (anode-parent2 AT) name))]
;                (cond [(and (empty? parent1) (empty? parent2)) empty]
;                      [(empty? parent1) (cons (anode-name AT) parent2)]
;                      [else (cons (anode-name AT) parent1)]))]))

;; This is a working solution, but define both local "parent1" and "parent2" under the same level
(define (get-f-descendants-path AT name)
  (local [(define (reverse-list/acc lst finallist)
            (cond [(empty? lst) finallist]
                  [else (reverse-list/acc (rest lst) (cons (first lst) finallist))]))
          (define (reverse-list lst)
            (reverse-list/acc lst empty))
          (define (get-f-descendants-path/1 AT name)
            (cond [(empty? AT) empty]
                  [(string=? name (anode-name AT)) (cons (anode-name AT) empty)]
                  [else (local [(define parent1 (get-f-descendants-path/1 (anode-parent1 AT) name))
                                (define parent2 (get-f-descendants-path/1 (anode-parent2 AT) name))]
                          (cond [(cons? parent1) (cons (anode-name AT) parent1)]
                                [(cons? parent2) (cons (anode-name AT) parent2)]
                                [else empty]))]))]
    (reverse-list (get-f-descendants-path/1 AT name))))

;; This is a working solution, but define both local "parent1" and "parent2" under the least required level
;(define (get-f-descendants-path AT name)
;  (local [(define (reverse-list/acc lst finallist)
;            (cond [(empty? lst) finallist]
;                  [else (reverse-list/acc (rest lst) (cons (first lst) finallist))]))
;          (define (reverse-list lst)
;            (reverse-list/acc lst empty))
;          (define (get-f-descendants-path/1 AT name)
;            (cond [(empty? AT) empty]
;                  [(string=? name (anode-name AT)) (cons (anode-name AT) empty)]
;                  [else (local [(define parent1 (get-f-descendants-path/1 (anode-parent1 AT) name))]
;                          (cond [(cons? parent1) (cons (anode-name AT) parent1)]
;                                [else (local [(define parent2 (get-f-descendants-path/1 (anode-parent2 AT) name))]
;                                        (cond [(cons? parent2) (cons (anode-name AT) parent2)]
;                                              [else empty]))]))]))]
;    (reverse-list (get-f-descendants-path/1 AT name))))

(check-expect (get-f-descendants-path copyright-free-ancestors "Arville") '("Arville" "Abrahum" "Homern't" "Liso"))
(check-expect (get-f-descendants-path copyright-free-ancestors "Liso") '("Liso"))
(check-expect (get-f-descendants-path copyright-free-ancestors "Homern't") '("Homern't" "Liso"))
(check-expect (get-f-descendants-path copyright-free-ancestors "Abrahum") '("Abrahum" "Homern't" "Liso"))
(check-expect (get-f-descendants-path copyright-free-ancestors "Mana") '("Mana" "Homern't" "Liso"))
(check-expect (get-f-descendants-path copyright-free-ancestors "Merge") '("Merge" "Liso"))
(check-expect (get-f-descendants-path copyright-free-ancestors "Cloncy") '("Cloncy" "Merge" "Liso"))
(check-expect (get-f-descendants-path copyright-free-ancestors "Bombi") '("Bombi" "Cloncy" "Merge" "Liso"))
(check-expect (get-f-descendants-path copyright-free-ancestors "Unknown") '())
(check-expect (get-f-descendants-path copyright-free-ancestors "Unknown") empty)

;(define ss1 (get-f-descendants-path copyright-free-ancestors "Arville"))

#|
;; Accumulateive reverse a lst
(define (reverse-list/acc lst finallist)
  (cond [(empty? lst) finallist]
        [else (reverse-list/acc (rest lst) (cons (first lst) finallist))]))

;; Wraper of reverse
(define (reverse-list lst)
  (reverse-list/acc lst empty))
  
(reverse-list ss1)
|#

;; Q2a
