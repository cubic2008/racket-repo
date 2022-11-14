;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bt2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
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