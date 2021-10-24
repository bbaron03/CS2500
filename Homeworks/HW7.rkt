;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname HW7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define WIDTH 400)
(define HEIGHT 400)

(define-struct world [size lives regions wall mouse])
; A World is a
; (make-world Posn Number [List-of Region] Wall Posn)
; INTERPRETATION: This keeps track of the size of the gameboard (in pixels),
; the number of lives the player has remaining, the collection of
; currently playable regions, the wall (if any), and the current position
; of the mouse cursor.  Note: All regions must have bounds between the origin
; and the size of this world.

(define-struct bounds [top-left bottom-right])
; A Bounds is a (make-bounds Posn Posn)
; INTERPRETATION: This records the coordinates of the top-left and
; bottom-right corners of each rectangular region of the board.

(define-struct region [bounds lob])
; A Region is a (make-region Bounds [List-of Ball])
; INTERPRETATION: This keeps track of the position and size of the region,
; and the balls bouncing within it.  

(define-struct vel [dx dy])
; A Velocity is a (make-vel Number Number)
; INTERPRETATION: The velocity of something (in pixels/tick) in the x and y
; directions
; Examples:
(define vel-1 (make-vel 5 10))
(define vel-2 (make-vel 0 3))
(define vel-3 (make-vel -4 -3))

; vel-temp: Velocity -> ?
(define (vel-temp vel)
  (... (vel-dx vel) ... (vel-dy vel) ...))

; A Posn is a (make-posn Number Number)
; INTERPRETATION: The position of something (in pixels), measured from (0,0)
; in the top-left corner, with +x pointing right and +y pointing down.
; Examples:
(define posn-0 (make-posn 0 0))
(define posn-1 (make-posn 160 HEIGHT))
(define posn-2 (make-posn 270 53))
(define posn-3 (make-posn 40 375))

; posn-temp: Posn -> ?
(define (posn-temp posn)
  (... (posn-x posn) ... (posn-y posn) ...))

(define-struct ball [pos vel])
; A Ball is a (make-ball Posn Velocity)
; INTERPRETATION: This records the current position (in pixels)
; and velocity (in pixels/tick) of an individual ball.
; Examples:
(define ball-1 (make-ball posn-1 vel-1))
(define ball-2 (make-ball posn-2 vel-2))
(define ball-3 (make-ball posn-3 vel-3))

; ball-temp: Ball -> ?
(define (ball-temp ball)
  (... (posn-temp (ball-posn ball)) ...
       ... (vel-temp (ball-vel ball)) ...))

; A Direction is one of:
; - "horizontal"
; - "vertical"
; Examples:
(define HORIZONTAL "horizontal")
(define VERTICAL "VERTICAL")

; direction-temp : Direction -> ?
(define (direction-temp direction)
  (cond [(string=? direction HORIZONTAL) ...]
        [(string=? direction VERTICAL) ...]))
        
(define-struct wall [direction top-left bottom-right])
; A Wall is one of:
; - Direction
; - (make-wall Direction Posn Posn)
; INTERPRETATION: Either a direction for the next wall that gets created,
; or the direction of growth, and the top-left and bottom-right corners of
; a growing wall.
; Examples:
(define WALL-THICKNESS 4)
(define BALL-RADIUS 5)

(define wall-1 (make-wall HORIZONTAL (make-posn 0 5) (make-posn 30 (+ 5 WALL-THICKNESS))))
(define wall-2 (make-wall VERTICAL (make-posn 150 100) (make-posn (+ 150 WALL-THICKNESS) 146)))

; wall-temp: Wall -> ?
(define (wall-temp wall)
  (... (wall-direction wall) ... (wall-top-left wall) ... (wall-bottom-right wall) ...))
 
