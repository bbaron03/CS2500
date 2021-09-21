;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Homework 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A PoundAmount is a postive real number
;; Interpretation: A PoundAmount is an amount of money in GBP
;; Examples:
(define POUNDAMOUNT1 45)
(define POUNDAMOUNT2 50)

; pound-temp: PoundAmount -> ?
(define (pound-temp pound-amount)
  (cond [(= pound-amount POUNDAMOUNT1) ...]
        [(= pound-amount POUNDAMOUNT2) ...]))

;; A EuroAmount is a positive real number
;; Interpretation: A EuroAmount is an amount of money in Euros
;; Examples:
(define EUROAMOUNT1 32)
(define EUROAMOUNT2 36)

; euro-temp EuroAmount -> ?
(define (euro-temp euro-amount)
  (cond [(= euro-amount EUROAMOUNT1) ...]
        [(= euro-amount EUROAMOUNT2) ...]))

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
(check-expect (gbp->eur POUNDAMOUNT1) EUROAMOUNT1)
(check-expect (gbp->eur POUNDAMOUNT2) EUROAMOUNT2)

(define (gbp->eur poundAmt)
  (- (* 4/5 poundAmt) 4))

;; Exercise 4 --------------------------------------------------

; encoding: String -> String
; Encodes a string by taking the capital first letter,
; the number of letters between the first and last letter,
; and the capital last letter
(check-expect (encoding "Northeastern") "N10N")
(check-expect (encoding "potter") "P4R")
(check-expect (encoding "brendan") "B5N")

(define (encoding word)
  (string-append (string-upcase (substring word 0 1 ))
                 (number->string (string-length (substring word 1 (- (string-length word) 1))))
                 (string-upcase (substring word (- (string-length word) 1) (string-length word)))))

;; Exercise 5 --------------------------------------------------
; A Year is a NaturalNumber
; Interpretation: The year that is being tested for being a leap year
; Examples
(define YEAR1 2004)
(define YEAR2 2008)
(define YEAR3 1990)
(define YEAR4 2000)

; year-temp -> Year -> ?
(define (year-temp year)
  (cond [(= year YEAR1) ...]
        [(= year YEAR2) ...]
        [(= year YEAR3) ...]
        [(= year YEAR4) ...]))
        
; leap-year: Year -> Boolean
; Determines if a Year is a leap year or not
(check-expect (leap-year? YEAR1) #true)
(check-expect (leap-year? YEAR2) #true)
(check-expect (leap-year? YEAR3) #false)
(check-expect (leap-year? YEAR4) #true)

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
(define MAYBEDICE-4 4)
(define MAYBEDICE-10 10)

; Template:
; maybeDice-temp: MaybeDice -> ?
(define (maybeDice-temp maybeDice)
  (cond [(boolean? maybeDice) ...]
        [(number? maybeDice) ...]))

; A DSG (DiceShuffleGame) is a (make-dsg MaybeDice MaybeDice MaybeDice)
(define-struct dsg [left middle right])
; and represents the three cups in a dice shuffle game, and what is under them
; Examples:
(define DSG1 (make-dsg #false 2 3))
(define DSG2 (make-dsg 10 2 20))
(define DSG3 (make-dsg 5 8 123))

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

(check-expect (shuffle-right (make-dsg MAYBEDICE-F MAYBEDICE-4 MAYBEDICE-10))
              (make-dsg MAYBEDICE-10 MAYBEDICE-F  MAYBEDICE-4))

; cup-value : DSG Guess -> MaybeDice
; Outputs the value of the dice in the cup at guess
(define (cup-value dsg guess)
  (cond [(string=? guess GUESS-LEFT) (dsg-left dsg)]
        [(string=? guess GUESS-MIDDLE) (dsg-middle dsg)]
        [(string=? guess GUESS-RIGHT) (dsg-right dsg)]))

(check-expect (cup-value DSG1 GUESS-LEFT)
              (dsg-left DSG1))
(check-expect (cup-value DSG2 GUESS-MIDDLE)
              (dsg-middle DSG2))
(check-expect (cup-value DSG3 GUESS-RIGHT)
              (dsg-right DSG3))

; extra-roll: DSG Number -> DSG
; Adds the number to each non-empty cup
(check-expect (extra-roll DSG1 5) (make-dsg #false 7 8))
(check-expect (extra-roll DSG2 10) (make-dsg 20 12 30))
(check-expect (extra-roll DSG3 1) (make-dsg 6 9 124))

(define (extra-roll dsg n)
  (make-dsg (cond [(boolean? (dsg-left dsg)) (dsg-left dsg)]
                  [(number? (dsg-left dsg)) (+ n (cup-value dsg GUESS-LEFT))])
             (cond [(boolean? (dsg-middle dsg)) (dsg-middle dsg)]
                  [(number? (dsg-middle dsg)) (+ n (cup-value dsg GUESS-MIDDLE))])
              (cond [(boolean? (dsg-right dsg)) (dsg-right dsg)]
                  [(number? (dsg-right dsg)) (+ n (cup-value dsg GUESS-RIGHT))])))

;; Exercise 11-20 Cowabunga ------------------------------------------
; A Height is a PositiveNumber;
; Interp; The length of the game screen in pixels
; Example:
(define HEIGHT 300)

; Template:
; height-temp: Height -> ?
(define (height-temp height)
  (cond [(number=? height HEIGHT) ...]))

; A Width is a PositiveNumber;
; Interp; The width of the game screen in pixels
; Example:
(define WIDTH 100)

; Template:
; width-temp Width -> ?
(define (width-temp width)
  (cond [(number=? width WIDTH) ...]))

; A UFO is a (make-posn Number Number)
; Interp: A (make-posn Number Number) has an x and y coordinate

; Examples
(define UFO (make-posn 10 10))

; Template:
; ufo-temp : UFO -> ?
(define (ufo-temp ufo)
  (... (posn-x ufo) ... (posn-y ufo) ...))

(define struct (make-cow x-cord isMovingLeft?))
; A Cow is a (make-cow Number Boolean)
; Interp: A (make-cow Number Boolean) is a cow who move left to right
; conntinously until a final game state is reached.

; Examples
(define COW1 (make-cow 0 #true))
(define COW2 (make-cow 40 #false))

; Template:
; cow-temp: Cow -> ?
(define (cow-temp cow)
  (... (cow-x-cord) ... (cow-isMovingLeft?)))

; ufo-down: UFO -> UFO
; Increases a UFO's y value a fixed amount
(define VERTICAL-SPEED 1)

(check-expect (ufo-down UFO) (make-posn (posn-x UFO) (+ VERTICAL-SPEED (posn-y UFO))))

(define (ufo-down ufo)
  (make-posn (posn-x UFO) (+ VERTICAL-SPEED (posn-y UFO))))

; ufo-left/right UFO KeyEvent -> UFO
; Determines which x direction the ufo should go in depending on the arrow pressed by the user

(define HORIZONTAL-SPEED 1)
(define LEFT "left")
(define RIGHT "right")

(check-expect (ufo-left/right UFO LEFT) (make-posn (- HORIZONTAL-SPEED (posn-x UFO)) (posn-y UFO)))
(check-expect (ufo-left/right UFO RIGHT) (make-posn (+ HORIZONTAL-SPEED (posn-x UFO)) (posn-y UFO)))

(define (ufo-left/right ufo key-event)
  (cond [(string=? key-event LEFT) (make-posn (- HORIZONTAL-SPEED (posn-x ufo)) (posn-y ufo))]
        [(string=? key-event RIGHT) (make-posn (+ HORIZONTAL-SPEED (posn-x ufo)) (posn-y ufo))]
        [else (make-posn (posn-x ufo) (posn-y ufo))]))

; move-cow: Cow -> Cow
; Moves the cow in the direction it is facing
(check-expect (move-cow COW1) (make-cow (- HORIZONTAL-SPEED (cow-x-cord COW1)) #true))
(check-expect (move-cow COW2) (make-cow (+ HORIZONTAL-SPEED (cow-x-cord COW2)) #false))

(define (move-cow cow)
  (cond [(boolean=? cow-isMovingLeft? #true)
         (make-cow (- HORIZONTAL-SPEED (cow-x-cord cow)) #true)]
        [(boolean=? cow-isMovingLeft? #false)
         (make-cow (+ HORIZONTAL-SPEED (cow-x-cord cow)) #false)]))

; cowOnEdge?: Cow -> Boolean
; Determines if the cow is at the edge of the screen
(check-expect (cowOnEdge? COW1) #true)
(check-expect (cowOnEdge? COW2) #false)

(define (cowOnEdge? cow)
  (cond [(or (>= WIDTH (cow-x-cord cow)) (<= (cow-x-cord cow) 0)) #true]
        [else #false]))

; flip-cow: Cow -> Cow
; Inverts the value of cow-isGoingLeft?
(check-expect (flip-cow COW1) (not (cow-isGoingLeft? COW1)))
(check-expect (flip-cow COW2) (not (cow-isGoingLeft? COW2)))

              