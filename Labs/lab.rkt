;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A FIAS (Finite Increasing Arithmetic Sequence) is a:
; (make-fias Number Number Positive)
(define-struct fias [min max step])
; where (make-fias min max step) represents all numbers
; of the form min + (k * step), where k is a natural number,
; such that min + (k * step) < max.
 
(define fias-empty (make-fias 1 1 0.25)) ; empty sequence, as min >= max
(define fias-1 (make-fias 0 1 0.25)) ; sequence with the elements (0, .25, .5, .75)
 
; fias-temp : FIAS -> ?
#;(define (fias-temp fias)
    (... (fias-min fias) ... (fias-max fias) ... (fias-step fias) ...))

; empty-fias? : FIAS -> Boolean
; determines if the given FIAS is empty
(define (empty-fias? fias)
  (>= (fias-min fias) (fias-max fias)))

; next-sequence : FIAS -> FIAS
; returns a new FIAS where the min is the original FIAS's min plus its step
(define (next-sequence fias)
  (make-fias
   (+ (fias-min fias) (fias-step fias))
   (fias-max fias)
   (fias-step fias)))

; fias-temp : FIAS -> ?
(define (fias-temp fias)
  (cond
    [(empty-fias? fias) ...]
    [else (... (fias-min fias) ... (fias-temp (next-sequence fias)) ...)]))

; sum-fias: FIAS -> Number
; Computes the sum of all numbers in a FIAS
(check-expect (sum-fias fias-empty) 0)
(check-expect (sum-fias fias-1) 1.5)

#; (define (sum-fias fias)
     (cond
       [(empty-fias? fias) 0]
       [else (+ (fias-min fias) (sum-fias (next-sequence fias)))]))

(define (sum-fias fias)
  (op-fias + fias 0))

; product-fias: FIAS -> Number
; Computes the product of all numbers in a FIAS
(check-expect (product-fias fias-empty) 1)
(check-expect (product-fias fias-1) 0)
(check-expect (product-fias (make-fias 1 5 1)) 24)

#; (define (product-fias fias)
     (cond
       [(empty-fias? fias) 1]
       [else (* (fias-min fias) (product-fias (next-sequence fias)))]))

(define (product-fias fias)
  (op-fias * fias 1))

; list-fias: FIAS -> LoX
; Creates a list from the elements of a FIAS
(check-expect (list-fias fias-empty) '())
(check-expect (list-fias fias-1) (list 0 .25 .5 .75))

#; (define (list-fias fias)
     (cond
       [(empty-fias? fias) '()]
       [else (cons (fias-min fias) (list-fias (next-sequence fias)))]))

(define (list-fias fias)
  (op-fias cons fias '()))

; op-fias: [X -> X] FIAS X-> X

(define (op-fias op fias base-case)
  (cond
    [(empty-fias? fias) base-case]
    [else (op (fias-min fias) (op-fias op (next-sequence fias) base-case))]))

; perfect-square?: FIAS -> Boolean
; Determines if any element of a FIAS is a perfect square
(check-expect (perfect-square? fias-empty) #false)
(check-expect (perfect-square? fias-1) #true)
(check-expect (perfect-square? (make-fias 1 5 1)) #true)

#;(define (perfect-square? fias)
    (local [(define sqrt-min (sqrt (fias-min fias)))]
      (cond [(empty-fias? fias) #false]
            [else (if (integer? sqrt-min)
                      #true
                      (perfect-square? (next-sequence fias)))])))

(define (perfect-square? fias)
  (local [(define (is-perfect-square? num)
            (integer? (sqrt num)))]
    (in-fias? is-perfect-square? fias)))
  
; even-in-fias?: FIAS -> Boolean
; Determines if any element of a FIAS is even
(check-expect (even-in-fias? fias-empty) #false)
(check-expect (even-in-fias? fias-1) #true)
(check-expect (even-in-fias? (make-fias 1 5 2)) #false)
(check-expect (even-in-fias? (make-fias 2 5 2)) #true)

#;(define (even-in-fias? fias)
    (local [(define min-fias (fias-min fias))]
      (cond [(empty-fias? fias) #false]
            [else (if (even? min-fias)
                      #true
                      (even-in-fias? (next-sequence fias)))])))

(define (even-in-fias? fias)
  (in-fias? even? fias))
  
; in-fias?: {X} [X-> Boolean] FIAS -> Boolean

(define (in-fias? compare fias)
  (local [(define min-fias (fias-min fias))]
    (cond [(empty-fias? fias) #false]
          [else (if (compare min-fias)
                    #true
                    (in-fias? compare (next-sequence fias)))])))
  
; keep-if: {X} [X -> Boolean] LoX -> LoX

(define (keep-if compare lox)
  (cond [(empty? lox) '()]
        [(cons? lox) (if (compare (first lox))
                         (cons (first lox) (keep-if compare (rest lox)))
                         (keep-if compare (rest lox)))]))

; keep-if-greater-than-10 : [List-of Number] -> [List-of Number]
; Keeps all elements of the list greater than 10
(check-expect (keep-if-greater-than-10 '()) '())
(check-expect (keep-if-greater-than-10 (list 1 10 100 -1)) (list 100))

(define (keep-if-greater-than-10 lox)
  (local [(define (greater-than-10 number)
            (> number 10))]
    (keep-if greater-than-10 lox)))

; keep-if-only-letters: [List-of String] -> [List-of String]
; Keeps all elements that only have letters in the string
(check-expect (keep-if-only-letters '()) '())
(check-expect (keep-if-only-letters (list "abc" "12a" "ui" "989")) (list "abc" "ui"))

(define (keep-if-only-letters lox)
  (keep-if string-alphabetic? lox))

; any-satisfy?: {X} [X -> Boolean] LoX -> Boolean

(define (any-satisfy? compare lox)
  (cond [(empty? lox) #false]
        [(cons? lox) (or (compare (first lox)) (any-satisfy? compare (rest lox)))]))

; are-any-even? : [List-of Number] -> Boolean
; Are any of the numbers even?
(define (are-any-even? lon)
  (any-satisfy? even? lon))

(check-expect (are-any-even? '()) #f)
(check-expect (are-any-even? (list 1 10 100 -1)) #t)
(check-expect (are-any-even? (list 1 15 -23)) #f)
 
; are-any-only-lowercase?: [List-of String] -> Boolean
; Are any strings in the list only lowercase?
(define (are-any-only-lowercase? los)
  (any-satisfy? string-lower-case? los))
  
(check-expect (are-any-only-lowercase? '()) #f)
(check-expect (are-any-only-lowercase? (list "abc" "12a" "ui" "989")) #t)
(check-expect (are-any-only-lowercase? (list "ABC" "CDE" "fgH")) #f)
