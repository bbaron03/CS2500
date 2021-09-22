;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A PoundAmount is a postive real number
;; Interpretation: A PoundAmount is an amount of money in GBP
;; Examples:
(define pnd1 45)
(define pnd2 50)

; pound-temp: PoundAmount -> ?
(define (pound-temp pound-amount)
  (... pound-amount ...))

;; A EuroAmount is a positive real number
;; Interpretation: A EuroAmount is an amount of money in Euros
;; Examples:
(define euro1 32)
(define euro2 36)

; euro-temp EuroAmount -> ?
(define (euro-temp euro-amount)
  (... euro-amount ...))

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
(check-expect (gbp->eur pnd1) euro1)
(check-expect (gbp->eur pnd2) euro2)

(define (gbp->eur poundAmt)
  (- (* 4/5 poundAmt) 4))

;; Exercise 4 --------------------------------------------------

;;A Word is a string of at least 2 letters and no other types of characters.

; word-temp Word -> ?
(define (word-temp word)
  (... word ...))

(define word1 "Northeastern")
(define word2 "potter")
(define word3 "brendan")

; encoding: Word -> String
; Encodes a string by taking the capital first letter,
; the number of letters between the first and last letter,
; and the capital last letter and putting it in a string
(check-expect (encoding word1) "N10N")
(check-expect (encoding word2) "P4R")
(check-expect (encoding word3) "B5N")

(define (encoding word)
  (string-append (string-upcase (substring word 0 1))
                 (number->string (string-length (substring word 1 (- (string-length word) 1))))
                 (string-upcase (substring word (- (string-length word) 1) (string-length word)))))

;; Exercise 5 --------------------------------------------------
; A Year is a NaturalNumber
; Interpretation: The year that is being tested for being a leap year
; Examples
(define year1 2004)
(define year2 2008)
(define year3 1990)
(define year4 2000)

; year-temp -> Year -> ?
(define (year-temp year)
  (... year ...))
        
; leap-year: Year -> Boolean
; Determines if a Year is a leap year or not
(check-expect (leap-year? year1) #true)
(check-expect (leap-year? year2) #true)
(check-expect (leap-year? year3) #false)
(check-expect (leap-year? year4) #true)

(define (leap-year? year)
  (cond [(= (modulo year 400) 0) #true]
        [(and (= (modulo year 4) 0) (not (= (modulo year 100) 0))) #true]
        [else #false]))

;; Exercise 6 --------------------------------------------------
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
        [(>= (modulo t FULL-CYCLE) HALF-CYCLE)
         (place-image BALL X (- YF (modulo t HALF-CYCLE)) BGD2)]))
 
(animate down-and-up)


;; Exercise 7, 8, 9, 10 --------------------------------------------------

; A MaybeDice is one of:
; - #false
; - Number
; and represents either no dice or the sum of die values

; Examples:
(define MAYBEDICE-F #false)
(define mbd-4 4)
(define mbd-10 10)

; Template:
; maybeDice-temp: MaybeDice -> ?
(define (maybeDice-temp maybeDice)
  (cond [(boolean? maybeDice) ...]
        [(number? maybeDice) ...]))

; A DSG (DiceShuffleGame) is a (make-dsg MaybeDice MaybeDice MaybeDice)
(define-struct dsg [left middle right])
; and represents the three cups in a dice shuffle game, and what is under them
; Examples:
(define dsg1 (make-dsg #false 2 3))
(define dsg2 (make-dsg 10 2 20))
(define dsg3 (make-dsg 5 8 123))

; Template:
; dsg-temp: DSG -> ?
(define (dsg-temp dsg)
  (... (dsg-left dsg) ... (dsg-middle dsg) ... (dsg-right dsg) ...))

; A Guess is one of:
; - "left"
; - "middle"
; - "right
; and represents the guess of which cup is being selected in the game
; Examples:
(define GUESS-LEFT "left")
(define GUESS-MIDDLE "middle")
(define GUESS-RIGHT "right")

; Template: 
; guess-temp: Guess -> ?
(define (guess-temp guess)
  (cond [(string=?  guess GUESS-LEFT) ...]
        [(string=?  guess GUESS-MIDDLE) ...]
        [(string=?  guess GUESS-RIGHT) ...]))

; shuffle-right : DSG -> DSG
; Shifts the dice values of a DSG to the right
(define (shuffle-right dsg)
  (make-dsg (dsg-right dsg) (dsg-left dsg) (dsg-middle dsg)))

(check-expect (shuffle-right (make-dsg MAYBEDICE-F mbd-4 mbd-10))
              (make-dsg mbd-10 MAYBEDICE-F  mbd-4))

; cup-value : DSG Guess -> MaybeDice
; Outputs the value of the dice in the cup at guess
(define (cup-value dsg guess)
  (cond [(string=? guess GUESS-LEFT) (dsg-left dsg)]
        [(string=? guess GUESS-MIDDLE) (dsg-middle dsg)]
        [(string=? guess GUESS-RIGHT) (dsg-right dsg)]))

(check-expect (cup-value dsg1 GUESS-LEFT)
              (dsg-left dsg1))
(check-expect (cup-value dsg2 GUESS-MIDDLE)
              (dsg-middle dsg2))
(check-expect (cup-value dsg3 GUESS-RIGHT)
              (dsg-right dsg3))

; extra-roll: DSG Number -> DSG
; Adds the number to each non-empty cup
(check-expect (extra-roll dsg1 5) (make-dsg #false 7 8))
(check-expect (extra-roll dsg2 10) (make-dsg 20 12 30))
(check-expect (extra-roll dsg3 1) (make-dsg 6 9 124))

(define (extra-roll dsg n)
  (make-dsg (cond [(boolean? (dsg-left dsg)) (dsg-left dsg)]
                  [(number? (dsg-left dsg)) (+ n (cup-value dsg GUESS-LEFT))])
            (cond [(boolean? (dsg-middle dsg)) (dsg-middle dsg)]
                  [(number? (dsg-middle dsg)) (+ n (cup-value dsg GUESS-MIDDLE))])
            (cond [(boolean? (dsg-right dsg)) (dsg-right dsg)]
                  [(number? (dsg-right dsg)) (+ n (cup-value dsg GUESS-RIGHT))])))

;; Exercise 11-20 Cowabunga ------------------------------------------

(define HEIGHT 300)
(define WIDTH 100)

; A UFO is a (make-posn Number Number)
; Interp: A UFO has an x and y coordinate
;in left-right top-down coordinate space represented by the posn.
;In the game, the UFO will have a circular hitbox. The position refers to the center of this circle
(define UFO-RAD 5)
; Examples
(define ufo1 (make-posn 10 10))
(define ufo2 (make-posn 75 (- HEIGHT UFO-RAD)))
(define ufo3 (make-posn 40 290))
; Template:
; ufo-temp : UFO -> ?
(define (ufo-temp ufo)
  (... (posn-x ufo) ... (posn-y ufo) ...))

(define-struct cow [x-cord isMovingLeft?])
; A Cow is a (make-cow Number Boolean)
; Interp: A (make-cow x b) is a cow with the position x, and is moving left
;if b is #true and right if b is #false
;In the game, the cow will have a rectangular hitbox.
;;The position refers to the bottom-left corner of the cow
(define COW-WIDTH 10)
(define COW-HEIGHT 5)
; Examples
(define cow1 (make-cow 0 #true))
(define cow2 (make-cow 40 #false))

; Template:
; cow-temp: Cow -> ?
(define (cow-temp cow)
  (... (cow-x-cord cow) ... (cow-isMovingLeft? cow ...)))

; ufo-down: UFO -> UFO
; Increases a UFO's y value a fixed amount
(define VERTICAL-SPEED 1)

(check-expect (ufo-down ufo1) (make-posn (posn-x ufo1) (+ VERTICAL-SPEED (posn-y ufo1))))

(define (ufo-down ufo)
  (make-posn (posn-x ufo) (+ VERTICAL-SPEED (posn-y ufo))))

; ufo-left/right UFO KeyEvent -> UFO
; Determines which x direction the ufo should go in depending on the arrow pressed by the user

(define UFO-HORIZONTAL-SPEED 2)
(define LEFT "left")
(define RIGHT "right")

(check-expect (ufo-left/right ufo1 LEFT)
              (make-posn (- UFO-HORIZONTAL-SPEED (posn-x ufo1)) (posn-y ufo1)))
(check-expect (ufo-left/right ufo1 RIGHT)
              (make-posn (+ UFO-HORIZONTAL-SPEED (posn-x ufo1)) (posn-y ufo1)))
(check-expect (ufo-left/right ufo1 "a") ufo1)

(define (ufo-left/right ufo key-event)
  (cond [(string=? key-event LEFT) (make-posn (- UFO-HORIZONTAL-SPEED (posn-x ufo)) (posn-y ufo))]
        [(string=? key-event RIGHT) (make-posn (+ UFO-HORIZONTAL-SPEED (posn-x ufo)) (posn-y ufo))]
        [else (make-posn (posn-x ufo) (posn-y ufo))]))

; move-cow: Cow -> Cow
; Moves the cow in the direction it is facing
(define COW-SPEED 1)

(check-expect (move-cow cow1) (make-cow (- COW-SPEED (cow-x-cord cow1)) #true))
(check-expect (move-cow cow2) (make-cow (+ COW-SPEED (cow-x-cord cow2)) #false))

(define (move-cow cow)
  (cond [(boolean=? (cow-isMovingLeft? cow) #true)
         (make-cow (- COW-SPEED (cow-x-cord cow)) #true)]
        [(boolean=? (cow-isMovingLeft? cow) #false)
         (make-cow (+ COW-SPEED (cow-x-cord cow)) #false)]))

; cowOnEdge?: Cow -> Boolean
; Determines if the cow is at the edge of the screen
(check-expect (cowOnEdge? cow1) #true)
(check-expect (cowOnEdge? cow2) #false)

(define (cowOnEdge? cow)
  (cond [(or (>= (cow-x-cord cow) (- WIDTH COW-WIDTH)) (<= (cow-x-cord cow) 0)) #true]
        [else #false]))

; flip-cow: Cow -> Cow
; Inverts the value of cow-isGoingLeft?
(check-expect (flip-cow cow1) (make-cow (cow-x-cord cow1) (not (cow-isMovingLeft? cow1))))
(check-expect (flip-cow cow2) (make-cow (cow-x-cord cow2) (not (cow-isMovingLeft? cow2))))

(define (flip-cow cow)
  (make-cow (cow-x-cord cow) (not (cow-isMovingLeft? cow))))

; cow-move-cycle Cow -> Cow
; Creates the cow move cycle that will flip the cow when necessary and continuously move the
; cow as well
(check-expect (cow-move-cycle cow1) (flip-cow cow1))
(check-expect (cow-move-cycle cow2) (move-cow cow2))

(define (cow-move-cycle cow)
  (cond [(cowOnEdge? cow) (flip-cow cow)]
        [else (move-cow cow)]))

; ufo-captured-cow? : UFO Cow -> Boolean
; Determines whether or not the UFO hitbox has collided with the cow hitbox and captured it

(check-expect (ufo-captured-cow? (make-posn 10 290) (make-cow 10 #false)) #true)
(check-expect (ufo-captured-cow? ufo1 cow1) #false)

(define (ufo-captured-cow? ufo cow)
  (cond [(and (<= (cow-x-cord cow) (posn-x ufo) (+ (cow-x-cord cow) COW-WIDTH))
              (= (posn-y ufo) (- HEIGHT (+ UFO-RAD COW-HEIGHT)))) #true]
        [else #false]))

; ufo-crashed? UFO -> Boolean
; Determies whether or not the UFO hitbox has collided with the ground
(check-expect (ufo-crashed? (make-posn 10 295)) #true)
(check-expect (ufo-crashed? ufo1) #false)

(define (ufo-crashed? ufo)
  (cond [(= (posn-y ufo) (- HEIGHT UFO-RAD)) #true]
        [else #false]))

; game-over? UFO Cow -> Boolean
; Runs both the ufo-captured-cow? and ufo-crashed? functions to determine when the game ends
(check-expect (game-over? ufo3 cow2) #true)
(check-expect (game-over? ufo2 cow1) #true)
(check-expect (game-over? ufo1 cow1) #false)

(define (game-over? ufo cow)
  (cond [(or (ufo-captured-cow? ufo cow) (ufo-crashed? ufo)) #true]
        [else #false]))
            



              