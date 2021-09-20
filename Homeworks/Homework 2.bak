;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A PoundAmount is a postive real number
;; Interpretation: A PoundAmount is an amount of money in GBP
;; Examples:
(define poundAmount1 10)
(define poundAmount2 15.50)

;; A EuroAmount is a positive real number
;; Interpretation: A EuroAmount is an amount of money in Euros
;; Examples:
(define euroAmount1 12.50)
(define euroAmount2 5)

;; gbp->eur : PoundAmount -> EuroAmount
;; Converts GBP to Euros by using the given chart
;; Examples: 
;; PoundAmount 5 10 15 20 25
;; EuroAmount  0  4  8 12 16
(check-expect (gbp->eur 5) 0)
(check-expect (gbp->eur 10) 4)
(check-expect (gbp->eur 15) 8)
(check-expect (gbp->eur 20) 12)
(check-expect (gbp->eur 25) 16)
(check-expect (gbp->eur 40) 28)

(define (gbp->eur poundAmt)
  (- (* 4/5 poundAmt) 4))

;; Exercise 4
(check-expect (encoding "Northeastern") "N10N")
(check-expect (encoding "potter") "P4R")
(check-expect (encoding "brendan") "B5N")


(define (encoding word)
  (string-append (string-upcase (substring word 0 1 ))
                 (number->string (string-length (substring word 1 (- (string-length word) 1))))
                 (string-upcase (substring word (- (string-length word) 1) (string-length word)))))

;; Exercise 5
(check-expect (leap-year? 2004) #true)
(check-expect (leap-year? 2008) #true)
(check-expect (leap-year? 1900) #false)
(check-expect (leap-year? 2000) #true)

(define (leap-year? year)
  (cond [(= (modulo year 400) 0) #true]
        [(and (= (modulo year 4) 0) (not (= (modulo year 100) 0))) #true]
        [else #false]))

;; Exercise 6
(require 2htdp/image)
(require 2htdp/universe)

(define RECT-HT 250)
(define BALL-RAD 25)
(define RECT-WD 50)
(define X BALL-RAD)
(define Y0 BALL-RAD)
(define YF (- RECT-HT BALL-RAD))
(define HALF-CYCLE (- RECT-HT (* 2 BALL-RAD)))
(define FULL-CYCLE (* 2 HALF-CYCLE))
(define BGD1 (rectangle RECT-WD RECT-HT "solid" "yellow"))
(define BGD2 (rectangle RECT-WD RECT-HT "solid" "orange"))
(define BALL (circle BALL-RAD "solid" "red"))

(define (down-and-up t)
  (cond [(< (modulo t FULL-CYCLE) HALF-CYCLE) (place-image BALL X (+ Y0 (modulo t HALF-CYCLE)) BGD1)]
        [(>= (modulo t FULL-CYCLE) HALF-CYCLE) (place-image BALL X (- YF (modulo t HALF-CYCLE)) BGD2)]))
 
(animate down-and-up)


;; Exercise 7, 8, 9, 10
; A DSG (DiceShuffleGame) is a (make-dsg MaybeDice MaybeDice MaybeDice)
(define-struct dsg [left middle right])
; and represents the three cups in a dice shuffle game, and what is under them

; Template:
(define (dsg-fn dsg)
  (... (dsg-left) ... (dsg-middle) ... (dsg-right) ...))

; A MaybeDice is one of:
; - #false
; - Number
; and represents either no dice or the sum of die values

; Examples:
(define MAYBEDICE-F #false)
(define MAYBEDICE-4 4)
(define MAYBEDICE-10 10)

; maybeDice-temp: MaybeDice -> ?
(define (maybeDice-temp maybeDice)
  (cond [(string=? maybeDice MAYBEDICE-F) ...]
        [(number? maybeDIce MAYBEDICE-4) ...]))

; A Guess is one of:
; - "left"
; - "middle"
; - "right

; Examples:
(define GUESS-LEFT "left")
(define GUESS-MIDDLE "middle")
(define GUESS-RIGHT "right")

; guess-temp: Guess -> ?
(define (guess-temp guess)
  (cond [(string=?  guess GUESS_LEFT) ...]
        [(string=?  guess GUESS-MIDDLE) ...]
        [(string=?  guess GUESS-RIGHT) ...]))

; shuffle-right : DSG -> DSG
; Shifts the dice values of a DSG to the right
(define (shuffle-right dsg)
  (make-dsg (dsg-right dsg) (dsg-left dsg) (dsg-middle dsg)))

(check-expect (shuffle-right (make-dsg MAYBEDICE-F MAYBEDICE-4 MAYBEDICE))
              (make-dsg maybeDice3 maybeDice1 maybeDice2))

; cup-value : DSG Guess -> MaybeDice value
; Outputs the value of the cup at
(define (cup-value dsg guess)
  (cond [(string=? guess "left") (dsg-left dsg)]
        [(string=? guess "middle") (dsg-middle dsg)]
        [(string=? guess "right") (dsg-right dsg)]))

(check-expect (cup-value (make-dsg #false 2 3) GUESS-LEFT)
              #false)
(check-expect (cup-value (make-dsg #false 2 3) GUESS-MIDDLE)
              2)
(check-expect (cup-value (make-dsg 5 8 123) GUESS-RIGHT)
              123)


;; Exercise 11-20
