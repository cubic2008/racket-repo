#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heap Support for Pretty Printing a Heap ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; by Dave Tompkins for CS135 in Fall 2022 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do NOT modify this file

;; ignore the #:transparent
(define-struct hnode (key left right) #:transparent)

;; A (heapof X) is one of:
;; * empty
;; * (make-hnode X (heapof X) (heapof X))
;; requires: all elements in left are >= key
;;           all elements in right are >= key


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    you do not need to understand the    ;;
;;  the following code, and please do not  ;;
;;  ask any questions on piazza about it   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (heap-print heap key->string) pretty prints the provided (heapof X)
;;   using the key->string function to convert type X to a string
;; NOTE: displays the heap rotated counter-clockwise of an angle of 90 degrees

;; heap-print: (heapof X) (X -> Str) -> Void
;; effects: displays the heap
(define (heap-print heap key->string)
  
  ;; (hp/acc h los) generates the string for heap-print and
  ;;   generates the left/right history for the heap in los
  ;; hp/acc: (heapof X) (listof (anyof 'L 'R)) -> Str
  (define (hp/acc h los)
    (cond [(empty? h) ""]
          [else (string-append (hp/acc (hnode-right h) (cons 'R los))
                               (hp/indent (reverse los)) (key->string (hnode-key h)) "\n"
                               (hp/acc (hnode-left h) (cons 'L los)))]))

  ;; (hp/indent los) produces a string showing the indentation/lines of the
  ;;   heap based on the L/R path in los
  ;; hp/indent (listof (anyof 'L 'R)) -> Str
  (define (hp/indent los)
    (cond [(empty? los) ""]
          [else (string-append 
                 (cond
                   ;; case 1+2: node with R/L elbows
                   [(and (empty? (rest los)) (symbol=? 'R (first los)))
                    (string #\u250c #\u2500)]
                   [(empty? (rest los)) (string #\u2514 #\u2500)]
                   ;; case 3: print vertical bar if two adjacent
                   ;;         L/R are mismatched
                   [(not (symbol=? (first los) (second los)))
                    (string #\u2502 #\space)]
                   ;; case 4: just whitespace
                   [else "  "])
                 (hp/indent (rest los)))]))

  (display (hp/acc heap empty)))

(provide heap-print make-hnode hnode? hnode-key hnode-left hnode-right)
