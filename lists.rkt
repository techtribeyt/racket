#lang racket

; car: get first element
; cdr: get all elements but the first element
; (list-ref lst i): gets us element at index i
; append: joins two lists
; cons: adds an element to list
; list: converts input into list
; empty? or null? checks for empty list
; esc-p lets you go up in console

; sum-all-els: adds all numbers in list
; (sum-all-els '(1 a (5 2 (4 r)) 3)) => 15
(define (sum-all-els lst)
  (cond
    [(empty? lst) 0]
    [(number? lst) lst]
    [(list? lst)
     (+ (sum-all-els (car lst)) (sum-all-els (cdr lst)))]
    [else 0]
  )
)

; function depth that returns depth of list
; (depth '(2 3 (5 (a b)))) => 3

(define (depth-aux lst max current)
  (cond
    [(empty? lst) (if (> current max) current max)]
    [(list? (car lst))
     (depth-aux (car lst) max (+ current 1))]
    [else (depth-aux (cdr lst) max current)]
   )
)

(define (depth lst)
  (cond
    [(empty? lst) 1]
    [else (depth-aux lst 0 1)]
   )
)


