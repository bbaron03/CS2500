;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; What is the world state?
; - the location of the food
; - the locations of the snake's segments  XXX (Segs)
; - the snake's direction


;;;;;;; DATA DEFINITIONS ;;;;;;;;;;
(define GRID-SQSIZE 10)
(define SEGS-RAD 5)
(define FOOD-IMG (circle SEGS-RAD "solid" "green"))
(define-struct snake [dir segs])
(define-struct world [snake food])
; A World is (make-world Snake Food)
; Interpretation: The world state of our game
; - snake is the current snake
; - food is the location of the current food
; Origin is at top left

; A Food is a (make-posn Number Number)
; Interpretation: The location of the food
; - x is the x-coordinate IN THE GRID of the food
; - y is the y-coordinate IN THE GRID of the food

; A Snake is a (make-snake Direction Segs)

; Interpretation: The current state of the snake
; - dir is the direction the snake is moving
; - segs is the location of the body segments of the snake
; A Segs is a NELOP
; The head is the first element in segs

(define (world-temp world)
  (... (snake-temp (world-snake world)) ... (posn-temp (world-food world)) ...))

(define (snake-temp snake)
  (... (dir-temp (snake-dir snake)) ... (nelop-temp (snake-segs snake)) ...))

(define (dir-temp dir)
  (cond [(string=? dir "up") ...]
        [(string=? dir "down") ...]
        [(string=? dir "left") ...]
        [(string=? dir "right") ...]))
        
(define (food-temp food)
  (... (posn-x food) ... (posn-y food) ...))

;; A NEListOfPosn (NELOP) is one of
;; (cons Posn '())
;; (cons Posn NELOP)

(define (nelop-temp nelop)
  (cond [(empty? (rest nelop))
         (... (posn-temp (first nelop)) ...)]
        [(cons? (rest nelop))
         (... (posn-temp (first nelop)) ... (nelop-temp (rest nelop)) ...)]))



;; move-snake : Snake -> Snake
;; Moves the snake by one cell in its current direction
(define (move-snake s)
  ;; HERE'S ONE IDEA -- DOES IT WORK?
  (chop-tail (add-head s)))

;; As discussed in class: chop-tail would remove the last segment of the snake,
;; and add-head would grow a new head segment onto the front of the snake.

;; CHALLENGE: Come up with *correct* signatures for chop-tail and add-head,
;; and any helper functions they need as well.  What goes wrong?

;;;;;;;;; EVENT HANDLERS ;;;;;;;

(define (snakegame _)
  (big-bang world1
    [to-draw world->scene]
    [on-tick world->World TICK-RATE]
    [on-key key-handler]
    [stop-when world-done?]))

; world->scene: World -> Scene
; Creates an image from a given world state
(define (world->scene world)
  (draw-snake-onto (world-snake w)
                   (draw-food-onto (world-food w)
                                   BACKGROUND)))

; draw-food-onto: Food Image -> Image
; Creates an image of the food on the another Image
(define (draw-food-onto food img)
  (place-image FOOD-IMG (posn-x food) (posn-y food) img))
; draw-snake-onto Snake Image -> Image
; Creates an image on the snake on another Image

; Place the top image onto the bottom image, at the grid coordinates specificed by x and y
; place-image-on-grid : Image Num Num Image -> Image
(define (place-image-on-grid top x y bot)
  