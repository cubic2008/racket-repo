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
(define sal-top-songs
  (list (list 1 "Respect")
        (list 3 "A Change is Gonna Come")
        (list 7 "Strawberry Fields Forever")
        (list 9 "Dreams")
        (list 12 "Superstition")
        (list 17 "Bohemian Rhapsody")
        (list 19 "Imagine")
        (list 21 "Strange Fruit")
        (list 25 "Runaway")
        (list 33 "Johnny B. Goode")))

(define-struct node (key val left right))

(define bstd-top-songs
  (make-node 12 "Superstition"
             (make-node 3 "A Change is Gonna Come"
                        (make-node 1 "Respect" empty empty)
                        (make-node 7 "Strawberry Fields Forever"
                                   empty
                                   (make-node 9 "Dreams" empty empty)))
             (make-node 21 "Strange Fruit"
                        (make-node 17 "Bohemian Rhapsody"
                                   empty
                                   (make-node 19 "Imagine" empty empty))
                        (make-node 25 "Runaway"
                                   empty
                                   (make-node 33 "Johnny B. Goode" empty empty)))))


;(define (sub-list/left lst median)
;  (cond [(or (empty? lst) (>= (first lst) median)) empty]
;        [else (cons (first lst) (sub-list/left (rest lst) median))]))
;(define (sub-list/right lst median)
;  (cond [(empty? lst) empty]
;        [(<= (first lst) median) (sub-list/right (rest lst) median)]
;        [else (cons (first lst) (sub-list/right (rest lst) median))]))
;(define ss1 '(1 2 3 4 5 6 7 9 11 20))
;(sub-list/left ss1 6)
;(sub-list/right ss1 6)

(define (build-bstd SAL)
  (local [
          (define (sub-list/left lst median)
            (cond [(or (empty? lst) (>= (first (first lst)) median)) empty]
                  [else (cons (first lst) (sub-list/left (rest lst) median))]))
          (define (sub-list/right lst median)
            (cond [(empty? lst) empty]
                  [(<= (first (first lst)) median) (sub-list/right (rest lst) median)]
                  [else (cons (first lst) (sub-list/right (rest lst) median))]))
          ]
    (cond [(empty? SAL) empty]
          [else (local [(define median (list-ref SAL (floor (- (/ (length SAL) 2) 0.5))))
                        (define left-list (sub-list/left SAL (first median)))
                        (define right-list (sub-list/right SAL (first median)))]
                  (make-node (first median) (second median) (build-bstd left-list) (build-bstd right-list)))])))


(check-expect (build-bstd sal-top-songs) bstd-top-songs)


;; Q2b
(define (range-query bstd k1 k2)
  (cond [(empty? bstd) empty]
        [(< (node-key bstd) k1) (range-query (node-right bstd) k1 k2)]
        [(> (node-key bstd) k2) (range-query (node-left bstd) k1 k2)]
;        [else (cons (range-query (node-left bstd) k1 k2) (cons (node-val bstd) (range-query (node-right bstd) k1 k2)))]))
        [else (append (range-query (node-left bstd) k1 k2) (cons (node-val bstd) (range-query (node-right bstd) k1 k2)))]))

;(range-query bstd-top-songs 3 18)
(check-expect (range-query bstd-top-songs 3 18) '("A Change is Gonna Come" "Strawberry Fields Forever" "Dreams" "Superstition" "Bohemian Rhapsody"))

;; Q3
(define-struct cnode (type id children))
(define npc2-aggressive-window-entry
  (make-cnode 'Sequence 1 (list "Walk to Window"
                                (make-cnode 'Selector 2 (list "Open Window"
                                                              (make-cnode 'Sequence 3 (list "Pick Window Lock"
                                                                                            "Open Window"))
                                                              "Smash Window"))
                                "Climb through Window"
                                "Turn Around"
                                (make-cnode 'Selector 4 (list "Close Window"
                                                              "Run Away")))))

(define npc1-through-window
  (make-cnode 'Sequence 1 (list "Walk to Window"
                                "Open Window"
                                "Climb through Window"
                                "Turn Around"
                                "Close Window")))

;; Q3a
(define (action-exists? BT action)
  (cond [(empty? BT) false]
        [else (local [(define (action-exists/lst? loBT action)
                        (cond [(empty? loBT) false]
                              [(cnode? (first loBT)) (or (action-exists? (first loBT) action) (action-exists/lst? (rest loBT) action))]
                              [(string=? (first loBT) action) true]
                              [else (action-exists/lst? (rest loBT) action)]))]
                (action-exists/lst? (cnode-children BT) action))]))

;(define (action-exists/lst? loBT action)
;  (cond [(empty? loBT) false]
;        [(symbol? (first loBT)) (action-exists? (first loBT) action)]
;        [(string=? (first loBT) action) true]
;        [else (action-exists/lst? (rest loBT) action)]))


(check-expect (action-exists? npc1-through-window "Open Door") false)
(check-expect (action-exists? npc1-through-window "Open Window") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Walk to Window") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Pick Window Lock") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Open Window") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Smash Window") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Climb through Window") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Turn Around") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Close Window") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Run Away") true)
(check-expect (action-exists? npc2-aggressive-window-entry "Unknown") false)
(check-expect (action-exists? npc2-aggressive-window-entry "") false)

;; Q3b

(define (summarize-bt BT)
  (local [(define (is-false? b s) (cond [(boolean=? b false) s] [else ""]))
          (define (summarize-bt/lst loBT and-or isFirst)
            (cond [(empty? loBT) ""]
                  [(cnode? (first loBT)) (string-append (is-false? isFirst and-or) (summarize-bt (first loBT)) (summarize-bt/lst (rest loBT) and-or false))]
                  [else (string-append (is-false? isFirst and-or) (first loBT) (summarize-bt/lst (rest loBT) and-or false))]))
          ]
    (cond [(empty? BT) ""]
          [(symbol=? (cnode-type BT) 'Sequence) (string-append "(" (summarize-bt/lst (cnode-children BT) " and " true) ")")]
          [else (string-append "(" (summarize-bt/lst (cnode-children BT) " or " true) ")")])))


;(define (is-false? b s) (cond [(boolean=? b false) s] [else ""]))
;
;(define (summarize-bt/lst loBT and-or isFirst)
;  (cond [(empty? loBT) ""]
;        [(cnode? (first loBT)) (string-append (is-false? isFirst and-or) (summarize-bt/local (first loBT) false) (summarize-bt/lst (rest loBT) and-or false))]
;        [else (string-append (is-false? isFirst and-or) (first loBT) (summarize-bt/lst (rest loBT) and-or false))]))

(check-expect (summarize-bt npc2-aggressive-window-entry)
"(Walk to Window and (Open Window or (Pick Window Lock and Open \
Window) or Smash Window) and Climb through Window and Turn \
Around and (Close Window or Run Away))")

;; Q3c
;(define (add-action a-cnode id action n)
;  (local [(define (find-cnode CNode)
;            (cond [(empty? CNode) empty] ;(make-cnode 'Sequence 1 (list action))]
;                  [(= (cnode-id CNode) id) (make-cnode (cnode-type CNode) (cnode-id CNode) (cons "found" (cnode-children CNode)))]
;                  [else (find-cnode/lst (cnode-children CNode))]))
;          (define (find-cnode/lst lon)
;            (cond [(empty? lon) empty] ;(make-cnode 'Sequence 1 (list action))]
;                  [(cnode? (first lon))
;                   (local [(define checked-node (find-cnode (first lon)))]
;                     (cond [(empty? checked-node) (find-cnode/lst (rest lon))]
;                           [else checked-node]
;                   ))]
;                  [else (find-cnode/lst (rest lon))]))
;;          (define (insert-node new-node pos)
;;            (cond [(empty? new-node) (make-cnode 'Sequence 1 (list action))]
;;                  [(= pos 1) (make]
;         ]
;    (find-cnode a-cnode)))

;(define (add-action a-cnode id action n)
;  (local [(define (find-cnode CNode)
;            (cond [(empty? CNode) empty] ;(make-cnode 'Sequence 1 (list action))]
;                  [(= (cnode-id CNode) id) (make-cnode (cnode-type CNode) (cnode-id CNode) (insert-to-list (cnode-children CNode) n))]
;                  [else (make-cnode (cnode-type CNode) (cnode-id CNode) (find-cnode/lst (cnode-children CNode)))]))
;          (define (insert-to-list los k)
;            (cond [(empty? los) (cons action empty)]
;                  [(= k 1) (cons action (rest los))]
;                  [else (cons (first los) (insert-to-list (rest los) (- k 1)))]))
;          (define (find-cnode/lst lon)
;            (cond [(empty? lon) empty] ;(make-cnode 'Sequence 1 (list action))]
;                  [(cnode? (first lon)) (cons (find-cnode (first lon)) (find-cnode/lst (rest lon)))]
;                  [else (cons (first lon) (find-cnode/lst (rest lon)))]
;                  ))
;         ]
;    (find-cnode a-cnode)))

(define (add-action a-cnode id action n)
  (local [(define (insert-to-list los k)
            (cond [(empty? los) (cons action empty)]
                  [(= k 1) (cons action (rest los))]
                  [else (cons (first los) (insert-to-list (rest los) (- k 1)))]))
          (define (find-cnode/lst lon)
            (cond [(empty? lon) empty]
                  [(cnode? (first lon)) (cons (add-action (first lon) id action n) (find-cnode/lst (rest lon)))]
                  [else (cons (first lon) (find-cnode/lst (rest lon)))]
                  ))
         ]
    (cond [(empty? a-cnode) empty]
          [(= (cnode-id a-cnode) id) (make-cnode (cnode-type a-cnode) (cnode-id a-cnode) (insert-to-list (cnode-children a-cnode) n))]
          [else (make-cnode (cnode-type a-cnode) (cnode-id a-cnode) (find-cnode/lst (cnode-children a-cnode)))])
))


(check-expect (add-action npc1-through-window 1 "Look through Window" 2)
              (make-cnode 'Sequence 1 (list "Walk to Window" "Look through Window" "Climb through Window" "Turn Around" "Close Window")))
(check-expect (add-action npc2-aggressive-window-entry 1 "Stop to Catch Breath" 3)
              (make-cnode
               'Sequence
               1
               (list
                "Walk to Window"
                (make-cnode 'Selector 2 (list "Open Window" (make-cnode 'Sequence 3 (list "Pick Window Lock" "Open Window")) "Smash Window"))
                "Stop to Catch Breath"
                "Turn Around"
                (make-cnode 'Selector 4 (list "Close Window" "Run Away")))))
(check-expect (add-action npc2-aggressive-window-entry 2 "Stop to Catch Breath" 3)
              (make-cnode 'Sequence 1
                          (list
                           "Walk to Window"
                           (make-cnode 'Selector 2
                                       (list "Open Window" (make-cnode 'Sequence 3 (list "Pick Window Lock" "Open Window")) "Stop to Catch Breath"))
                           "Climb through Window"
                           "Turn Around"
                           (make-cnode 'Selector 4 (list "Close Window" "Run Away")))))
(check-expect (add-action npc2-aggressive-window-entry 3 "Stop to Catch Breath" 3)
              (make-cnode
               'Sequence
               1
               (list
                "Walk to Window"
                (make-cnode
                 'Selector
                 2
                 (list "Open Window" (make-cnode 'Sequence 3 (list "Pick Window Lock" "Open Window" "Stop to Catch Breath")) "Smash Window"))
                "Climb through Window"
                "Turn Around"
                (make-cnode 'Selector 4 (list "Close Window" "Run Away")))))
(check-expect (add-action npc2-aggressive-window-entry 4 "Stop to Catch Breath" 3)
              (make-cnode
               'Sequence
               1
               (list
                "Walk to Window"
                (make-cnode 'Selector 2 (list "Open Window" (make-cnode 'Sequence 3 (list "Pick Window Lock" "Open Window")) "Smash Window"))
                "Climb through Window"
                "Turn Around"
                (make-cnode 'Selector 4 (list "Close Window" "Run Away" "Stop to Catch Breath")))))
(check-expect (add-action npc2-aggressive-window-entry 10 "Stop to Catch Breath" 3) npc2-aggressive-window-entry)

;; Q3d
(define (rewind BT)
  (cond [(empty? BT) empty]
        [else (local [(define (rewind/lst lon)
                        (cond [(empty? lon) empty]
                              [(cnode? (first lon)) (append (rewind/lst (rest lon)) (rewind (first lon)))]
                              [else (append (rewind/lst (rest lon)) (list (first lon)))]))]
         (rewind/lst (cnode-children BT)))]))



(check-expect (rewind npc1-through-window)
              (list "Close Window" "Turn Around" "Climb through Window" "Open Window" "Walk to Window"))
(check-expect (rewind npc2-aggressive-window-entry)
              (list "Run Away" "Close Window" "Turn Around" "Climb through Window"
                    "Smash Window" "Open Window" "Pick Window Lock" "Open Window"
                    "Walk to Window"))

;; Q4b

(define (normalize lon)
  (local [(define (max a b) (cond [(> a b) a] [else b]))
          (define (min a b) (cond [(< a b) a] [else b]))
          (define (calc-aggregate lst agg)
            (cond [(empty? lst) agg]
                  [else (calc-aggregate (rest lst) (list (max (first agg) (first lst)) (min (second agg) (first lst)) (add1 (third agg))))]))
          (define agg-value (calc-aggregate lon (list (first lon) (first lon) 0)))
          (define max-value (first agg-value))
          (define min-value (second agg-value))
          (define max-min-value (- max-value min-value))
          (define (normalize/helper lst)
            (cond [(empty? lst) empty]
                  [else (cons (/ (- (first lst) min-value) max-min-value) (normalize/helper (rest lst)))]))
          ]
;    (list agg-value max-value min-value max-min-value)))
;    (cond [(empty? lon) empty]
;;          [else (cons (/ (- (first lon) min-value) max-min-value) (normalize (rest lon)))])))
;          [else (cons max-min-value (normalize (rest lon)))])))
    (normalize/helper lon)))

(check-expect (normalize '(2 4 6)) '(0 0.5 1))
