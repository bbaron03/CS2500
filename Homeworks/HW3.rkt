;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; FROM HOMEWORK 2 ------------------------------------------

(define HEIGHT 300)
(define WIDTH 100)

(define-struct cow [x-cord isMovingLeft?])
; A Cow is a (make-cow Number Boolean)
; Interp: A (make-cow x b) is a cow with the position x, and is moving left
;if b is #true and right if b is #false
;In the game, the cow will have a rectangular hitbox.
;;The position refers to the bottom-left corner of the cow
(define COW-WIDTH 10)
(define COW-HEIGHT 5)

; Examples
(define cow1 (make-cow (/ COW-WIDTH 2) #true))
(define cow2 (make-cow 40 #false))
(define cow3 (make-cow 50 #true))

; Template:
; cow-temp: Cow -> ?
(define (cow-temp cow)
  (... (cow-x-cord cow) ... (cow-isMovingLeft? cow ...)))

; A UFO is a (make-posn Number Number)
; Interp: A UFO has an x and y coordinate
;in left-right top-down coordinate space represented by the posn.
;In the game, the UFO will have a circular hitbox. The position refers to the center of this circle
(define UFO-RAD 5)

; Examples
(define ufo1 (make-posn 10 10))
(define ufo2 (make-posn 40 (- HEIGHT COW-HEIGHT)))
(define ufo3 (make-posn 75 (- HEIGHT UFO-RAD)))

; Template:
; ufo-temp : UFO -> ?
(define (ufo-temp ufo)
  (... (posn-x ufo) ... (posn-y ufo) ...))

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
              (make-posn (- (posn-x ufo1) UFO-HORIZONTAL-SPEED) (posn-y ufo1)))
(check-expect (ufo-left/right ufo1 RIGHT)
              (make-posn (+ UFO-HORIZONTAL-SPEED (posn-x ufo1)) (posn-y ufo1)))
(check-expect (ufo-left/right ufo1 "a") ufo1)

(define (ufo-left/right ufo key-event)
  (cond [(string=? key-event LEFT) (make-posn (- (posn-x ufo) UFO-HORIZONTAL-SPEED) (posn-y ufo))]
        [(string=? key-event RIGHT) (make-posn (+ UFO-HORIZONTAL-SPEED (posn-x ufo)) (posn-y ufo))]
        [else (make-posn (posn-x ufo) (posn-y ufo))]))

; move-cow: Cow -> Cow
; Moves the cow in the direction it is facing
(define COW-SPEED 1)

(check-expect (move-cow cow1) (make-cow (- (cow-x-cord cow1) COW-SPEED) #true))
(check-expect (move-cow cow2) (make-cow (+ COW-SPEED (cow-x-cord cow2)) #false))

(define (move-cow cow)
  (cond [(cow-isMovingLeft? cow)
         (make-cow (- (cow-x-cord cow) COW-SPEED) #true)]
        [else (make-cow (+ COW-SPEED (cow-x-cord cow)) #false)]))

; cowOnEdge?: Cow -> Boolean
; Determines if the cow is at the edge of the screen
(check-expect (cowOnEdge? cow1) #true)
(check-expect (cowOnEdge? cow2) #false)

(define (cowOnEdge? cow)
  (or (= (cow-x-cord cow) (- WIDTH (/ COW-WIDTH 2))) (= (cow-x-cord cow) (/ COW-WIDTH 2))))

; flip-cow: Cow -> Cow
; Inverts the value of cow-isGoingLeft?
(check-expect (flip-cow cow1) (make-cow (cow-x-cord cow1) (not (cow-isMovingLeft? cow1))))
(check-expect (flip-cow cow2) (make-cow (cow-x-cord cow2) (not (cow-isMovingLeft? cow2))))

(define (flip-cow cow)
  (make-cow (cow-x-cord cow) (not (cow-isMovingLeft? cow))))

; cow-move-cycle Cow -> Cow
; Creates the cow move cycle that will flip the cow when necessary and continuously move the
; cow as well
(check-expect (cow-move-cycle cow1) (move-cow (flip-cow cow1)))
(check-expect (cow-move-cycle cow2) (move-cow cow2))

(define (cow-move-cycle cow)
  (if (cowOnEdge? cow) (move-cow (flip-cow cow)) (move-cow cow)))

; ufo-captured-cow? : UFO Cow -> Boolean
; Determines whether or not the UFO hitbox has collided with the cow hitbox and captured it

(check-expect (ufo-captured-cow? (make-posn 10 (- HEIGHT COW-HEIGHT)) (make-cow 10 #false)) #true)
(check-expect (ufo-captured-cow? ufo1 cow1) #false)

(define (ufo-captured-cow? ufo cow)
  (and (<= (cow-x-cord cow) (posn-x ufo) (+ (cow-x-cord cow) COW-WIDTH))
       (>= (sub1 HEIGHT) (posn-y ufo) (- HEIGHT (+ UFO-RAD COW-HEIGHT)))))

; ufo-crashed? UFO -> Boolean
; Determies whether or not the UFO hitbox has collided with the ground
(check-expect (ufo-crashed? (make-posn 10 (- HEIGHT UFO-RAD))) #true)
(check-expect (ufo-crashed? ufo1) #false)

(define (ufo-crashed? ufo)
  (>= (posn-y ufo) (- HEIGHT UFO-RAD)))


;;;;;;;;;;;;;;; ^^^HOMEWORK 2 ^^^^;;;;;;;;;;;;;;;;;;

(define-struct world [cow ufo])
; A World is a (make-world Cow UFO)
; Interp: A (make-world c u) is 
; the state of the Cowabunga game that contains a
; Cow c and a UFO u.

; Examples
(define world-1 (make-world cow1 ufo1))
(define world-2 (make-world cow2 ufo2))
(define world-3 (make-world cow3 ufo3))

; Template
; world-temp: Cow UFO -> ?
(define (world-temp world)
  (... (cow-temp (world-cow world)) ... (ufo-temp (world-ufo world)) ...))

; game-over? World -> Boolean
; Runs both the ufo-captured-cow? and ufo-crashed? functions to determine when the game ends
(check-expect (game-over? world-1) #false)
(check-expect (game-over? world-2) #true)
(check-expect (game-over? world-3) #true)

(define (game-over? world)
  (or (ufo-captured-cow? (world-ufo world)
                         (world-cow world))
      (ufo-crashed? (world-ufo world))))

; key-handler: World KeyEvent -> World
; Determines the next location of a UFO given a certain KeyEvent
(define world-ufo-edge1 (make-world cow1 (make-posn UFO-RAD 50)))
(define world-ufo-edge2 (make-world cow1 (make-posn (- WIDTH UFO-RAD) 50)))

(check-expect (key-handler world-1 "right")
              (make-world cow1 (ufo-left/right (world-ufo world-1) "right")))
(check-expect (key-handler world-ufo-edge1 "left") world-ufo-edge1)
(check-expect (key-handler world-ufo-edge2 "left")
              (make-world cow1 (ufo-left/right (world-ufo world-ufo-edge2) "left")))
(check-expect (key-handler world-1 "up") world-1)

(define (key-handler world key-event)
  (cond [(and (string=? "left" key-event) (> (posn-x (world-ufo world)) UFO-RAD))
         (make-world (world-cow world) (ufo-left/right (world-ufo world) key-event))]
        [(and (string=? "right" key-event) (< (posn-x (world-ufo world)) (- WIDTH UFO-RAD)))
         (make-world (world-cow world) (ufo-left/right (world-ufo world) key-event))]
        [else world]))

; world-render: World -> Image
; Renders the images in the world from a given World
(define COW-IMAGE (rectangle COW-WIDTH COW-HEIGHT "solid" "brown"))
(define UFO-IMAGE (circle UFO-RAD "solid" "gray"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(check-expect (world-render world-1)
              (place-image COW-IMAGE (cow-x-cord (world-cow world-1)) (- HEIGHT COW-HEIGHT)
                           (place-image UFO-IMAGE (posn-x (world-ufo world-1))
                           (posn-y (world-ufo world-1)) BACKGROUND)))

(define (world-render world)
  (place-image COW-IMAGE (cow-x-cord (world-cow world)) (- HEIGHT COW-HEIGHT)
                           (place-image UFO-IMAGE (posn-x (world-ufo world))
                           (posn-y (world-ufo world)) BACKGROUND)))

; move-on-tick: World -> World
; Moves the UFO and Cow automatically per tick
(check-expect (move-on-tick world-1)
              (make-world (cow-move-cycle (world-cow world-1)) (ufo-down (world-ufo world-1))))
(check-expect (move-on-tick world-2)
              (make-world (cow-move-cycle (world-cow world-2)) (ufo-down (world-ufo world-2))))
(check-expect (move-on-tick world-3)
              (make-world (cow-move-cycle (world-cow world-3)) (ufo-down (world-ufo world-3))))

(define (move-on-tick world)
  (make-world (cow-move-cycle (world-cow world)) (ufo-down (world-ufo world))))

; cowabunga: World -> World
; Starts and runs the cowabunga big-bang game with a given world-state, stops
; when the end conditions are met

(define (cowabunga world)
  (big-bang world [on-tick move-on-tick]
                  [to-draw world-render]
                  [on-key key-handler]
                  [stop-when game-over?]))

; Bugs as of Sunday
; - Cow flashes in and out when moving left

;; Guess My Number

(define goal-number (random 10))

;; You call that a Pizza?

; A Pizza is one of:
; - "red"
; - "no"
; - (make-topping String Pizza)
(define-struct topping [name more])
; and represents a collection of toppings on a pizza

; Examples
(define pizza1 "red")
(define pizza2 "no")
(define pizza3 (make-topping "salami" pizza2))
(define pizza4 (make-topping "bacon" pizza3))

;Template
; pizza-temp: Pizza -> ?
#;(define (pizza-temp pizza)
    (cond [(and (string? pizza) (string=? pizza "red")) ...]
          [(and (string? pizza) (string=? pizza "no")) ...]
          [(topping? pizza) ... (pizza-temp (topping-more pizza)) ...]))

; describe: Pizza -> String
; Outputs all of the toppings on a given pizza
(check-expect (describe pizza1) "red sauce")
(check-expect (describe pizza2) "no sauce")
(check-expect (describe pizza3) "salami and no sauce")
(check-expect (describe pizza4) "bacon and salami and no sauce")

(define (describe pizza)
  (cond [(and (string? pizza) (string=? pizza "red")) "red sauce"]
        [(and (string? pizza) (string=? pizza "no")) "no sauce"]
        [(topping? pizza) (string-append (topping-name pizza) " and "
                          (describe (topping-more pizza)))]))

; header: Pizza -> String
; Combines the first part of the description sentence with the part that describe creates,
; which is the toppings and the sauce on given pizza
(check-expect (header pizza1) "This pizza has red sauce")
(check-expect (header pizza2) "This pizza has no sauce")
(check-expect (header pizza3) "This pizza has salami and no sauce")
(check-expect (header pizza4) "This pizza has bacon and salami and no sauce")

(define (header pizza)
  (string-append "This pizza has " (describe pizza)))

;; Tower of Terror

; A Building is one of:
; - "ground"
; - (make-story Number PosInt String Building)
(define-struct story [height rooms color below])
; and represents either the ground story,
; or a story with a height, number of rooms, color, and
;   the rest of the building beneath it

; Examples
(define building1 "ground")
(define building2 (make-story 20 3 "red" building1))
(define building3 (make-story 40 4 "orange" building2))

; Template
; building-temp: Building -> ?
(define (building-temp building)
  (... cond [(string? building) ...]
        [(story? building) ... (building-temp (story-below building)) ...]))

; sum-rooms : Building -> PosInt
; Computes the total sum of the rooms in the building
(check-expect (sum-rooms building1) 0)
(check-expect (sum-rooms building2) 3)
(check-expect (sum-rooms building3) 7)

(define (sum-rooms building)
  (cond [(string? building) 0]
        [(story? building) (+ (story-rooms building)
                              (sum-rooms (story-below building)))]))

; draw-building : Building -> Image
; Draws a given building and all of its stories
(define BUILD-WIDTH 20)
(check-expect (draw-building building1) empty-image)
(check-expect (draw-building building2)
              (above (frame (rectangle BUILD-WIDTH 20  "solid" "red")) empty-image))
(check-expect (draw-building building3)
              (above (frame (rectangle BUILD-WIDTH 40 "solid" "orange")) 
                     (above (frame (rectangle BUILD-WIDTH 20 "solid" "red")) empty-image)))

(define (draw-building building)
  (cond [(string? building) empty-image]
        [(story? building)
         (above (frame (rectangle BUILD-WIDTH (story-height building) "solid" (story-color building)))
                (draw-building (story-below building)))]))