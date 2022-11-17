;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname heap) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; make sure heap-support.rkt is in the same folder as this file (heap.rkt)
(require "heap-support.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Do NOT uncomment the following (define-struct hnode ...) because it is
;; defined for you in the (require "heap-support.rkt") above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-struct hnode (key left right))
;;
;; A (heapof X) is one of:
;; * empty
;; * (make-hnode X (heapof X) (heapof X))
;; requires: all elements in left are >= key
;;           all elements in right are >= key
;;
;; The following function is available for your use:
;; 
;; (heap-print heap key->string) pretty prints the provided (heapof X)
;;   using the key->string function to convert type X to a string
;; NOTE: displays the heap rotated counter-clockwise of an angle of 90 degrees
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; uncomment this line to "print" the example heap in the interactions window
;; this function may help you to debug your code
;; NOTE: do NOT leave in any (heap-print ...) calls in your code when submitting
(heap-print example-heap number->string)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;












