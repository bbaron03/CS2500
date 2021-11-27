;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Processing2ComplexInputs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Simulatenously processing 2 complex inputers

;; Case 1

; append-lists: [List-of Number] [List-of Number] -> [List-of Number]
; Appends the two lists together

(check-expect (append-lists '() '()) '())
(check-expect (append-lists (list 1 2 3) (list 4 5)) (list 1 2 3 4 5))

(define (append-lists lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(cons? lon1) (cons (first lon1) (append-lists (rest lon1) lon2))]))
  
;;----------------------------------------

;; at-end : [List-of X] [List-of X] -> [List-of X]
;; replace the '() at end of ltwo with lone
(check-expect (at-end '() '()) '())
(check-expect (at-end '(a b c) '(d e f)) '(d e f a b c))
                     
(define (at-end lon1 lon2)
  (cond [(empty? lon2) lon1]
        [(cons? lon2) (cons (first lon2) (at-end lon1 (rest lon2)))]))


;;--------------------------------------------------------------------------------

;; Case 2

;--------------------------------------------------------------------------------
;; find: Symbol [List-of Symbol] [List-of X] -> X
;; produce the item in meanings that corresponds to s in names
;; Assume naems and meanings are of same length
;; i.e. Assume (= (length names) (lenght meanings))

(check-expect (find 'b '(a b c) '(1 2 3)) 2)
(check-error (find 'b '() '()) "Cant find b")
(check-error (find 'z '(a b c) '(1 2 3)) "Cant find z")

(define (find s names meanings)
  (cond [(empty? names) (error (string-append "Cant find " (symbol->string s)))]
        [(cons? names) (if (symbol=? s (first names))
                           (first meanings)
                           (find s (rest names) (rest meanings)))]))