;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Sample Problem Define a function that accepts a
;number of minutes and computes how many hours these minutes represent.

(define (number-of-hours minutes)
  (/ minutes 60)
  )


; Sample Problem Define the function how-far which consumes the
; number of minutes you drive at 55mph and produces how far you get in the given time.

(define SPEED 55)
(define (how-far minutes)
  (* (number-of-hours minutes) SPEED)
  )

; Exercise 7 Define a function multiple-of-5? that accepts a number and determines if it is a multiple 5.
(define (multiple-of-5 n)
  (if (= (modulo n 5) 0) true false)
  )

; Exercise 9 Define a constant GREETING which contains your greeting. Then, use it in a function called greet that takes a name and outputs a full greeting.
(define GREETING "Hello World!")
(define (greet name)
  (string-append GREETING "Nice to meet you too, " name "!")
  )
; Exercise 10 Call these functions in the interactions window.
; What happens if you call (multiple-of-5? "dog") or (greet #t)?

;; (multiple-of-5 "dog") Throws an error because the function only is accepting an integer
;; (greet #t) #t is a boolean value, the function is expecting a string value

; Exercise 11 Call your functions in the definitions window.
; Run the stepper on your program. Follow it through to see how it evaluates.

; Exercise 12 It has been estimated that fixing the water in Flint, Michigan would cost 55 million dollars.
; Given someone’s net worth, define a function that will determine what percentage of their net worth it would take to fix the water.
; Elon Musk’s net worth, for example, is 77.2 billion and Jeff Bezos’ is 187.8 billion.

(define (percent-net-worth ntWrth)
  (* 100 (/ 55000000 ntWrth))
  )

; Exercise 13 Define the function absolute which consumes a number
; and produces the absolute value of that number (do not use abs).


(define (absolute n)
  (if (> n 0) n (- 0 n)) ; If n is greater than 0, return n, else return -n
  )

; Starter Code: Below is a data definition that defines what a Score can be.
; A Score is a number in the range [0, 100]                     

; Exercise 14 Define the function letter-grade. It consumes a Score and produces
;the corresponding letter grade ("A", "B", "C", "D", or "F"). You may be as wishful as
;desired in choosing your own grading scale.

(define (letter-grade score)
  (cond
    [(< 65 score) "F"]
    [(<= 75 score) "D"]
    [(<= 80 score) "C"]
    [(< 90 score) "B"]
    [(< 100 score) "A"]
    )
  )

; Exercise 15 Define a function that given a number of hours worked and an hourly wage
;computes how much money has been earned. Any hours worked over 40 hours must be paid at 1.5 times the given rate.

(define (money-earned hours wage)
  (cond
    [(> hours 40) (* (- hours 40) (* wage 1.5))]
    [(<= hours 40) (* hours wage)]
    )
  )
; Exercise 16 Define the function runtime that given a movie title outputs
; its runtime in minutes. Ensure the function works on a handful of inputs of your choosing,
; and for any other inputs throws an error.

(define (runtime movie)
  (cond
    ["lion king" 88]
    ["toy story 2" 92]
    ["inception" 148]
    ["fight club" 139]
    )
)

; Starter Code: This line of code is required to program with images.
(require 2htdp/image)

; Exercise 17 Use triangle, square, rectangle, above, and overlay/align to draw yourself a
; house with a roof and door (and circle if you’re feeling bold enough for a door handle).
; Exercise 18 Congratulations, you’ve moved up a tax bracket. Define a constant WINDOW and
; place two of them on your humble home. Note how in using a constant we only have to draw it once and get to use it twice!


(define WINDOW (square 75 "solid" "lightblue"))
(place-image WINDOW 75 350
               (place-image WINDOW 230 350
                              (above (triangle 330 "solid" "red")
                                     (overlay/align "middle" "bottom"
                                                    (square 100 "solid" "brown")
                                                    (rectangle 300 250 "solid" "white"))
       
                                     )
              )
    )



; Exercise 19 Define a function scene that takes in a natural number and places a circle of that radius at the
; center of a 50x50 empty-scene. Use modulo to ensure the radius always stays below 20.
(define (radius t)
  (if (< (modulo t 40) 20) (modulo t 40) (- 20 (modulo t 20)))
  )
(define (scene t)
  (overlay (circle (radius t) "solid" "red") (empty-scene 50 50))
 )
; Starter Code: These lines of code will turn your scene function into an animation:
(require 2htdp/universe)

; Exercise 20 Launch your animation by pressing run.
(animate scene)
