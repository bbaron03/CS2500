;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
; A Shape is one of:
; - "circle"
; - "square"
; - "triangle"
; and represents a kind of shape

; Examples
(define CIRCLE "circle")
(define SQUARE "square")
(define TRIANGLE "triangle")
 
; Template
(define (shape-temp shape)
  (cond [(string=? shape "circle")   ...]
        [(string=? shape "square")   ...]
        [(string=? shape "triangle") ...]))

; draw: Shape -> Image
; Draw the shape
(define (draw shape)
  (cond [(string=? shape "circle")   (circle 10 "solid" "red")]
        [(string=? shape "square")   (square 5 "solid" "blue")]
        [(string=? shape "triangle") (triangle 7 "solid" "orange")]))
 
(check-expect (draw CIRCLE)   (circle 10 "solid" "red"))
(check-expect (draw SQUARE)   (square 5 "solid" "blue"))
(check-expect (draw TRIANGLE) (triangle 7 "solid" "orange"))

; Worming as Desinged

(define-struct worm (its-name head body))
(define-struct head (mouth radius))

; A Mouth is one of:
; – "open"
; – "closed"
; Interpretation: Is a mouth for a worm that is either open or closed
; Examples
(define MOUTH-OPEN "open")
(define MOUTH-CLOSED "closed")

; Template: 
; mouth-temp: Mouth -> ?
(define (mouth-temp mouth)
  (cond [(string=? mouth MOUTH-OPEN) ...]
        [(string=? mouth MOUTH-CLOSED) ...]))

; A Head is (make-head Mouth PositiveNumber).
; A (make-head Mouth PositiveNumber) represents a head with a mouth of Radius PositiveNumber

(define HEAD-RAD 25)
(define HEAD-OPEN (make-head MOUTH-OPEN HEAD-RAD))
(define HEAD-CLOSED (make-head MOUTH-CLOSED HEAD-RAD))

; head-temp: Head -> ?
(define (head-temp head)
  (... (mouth-temp (head-mouth head)) ... (head-radius) ...))

; A Worm is (make-worm String Head PositiveNumber).
; Interpretation: Represents a worm of name String, head Head and length PositiveNumber
; Examples:

(define WORM-BODY 30)
(define WORM-OPEN (make-worm "Sherry" HEAD-OPEN WORM-BODY))
(define WORM-CLOSED (make-worm "Jerry" HEAD-CLOSED WORM-BODY))

; Template:
; worm-temp: Worm -> ?
(define (worm-temp worm)
  (... (worm-its-name worm) ... (head-temp (worm-head worm)) ... (worm-body worm)))

; describe: Worm -> String
; Takes in a worm and outputs a string including the
; worms name and mouth state

(check-expect (describe WORM-OPEN) (string-append (worm-its-name WORM-OPEN)
               "'s mouth looks " (head-mouth (worm-head WORM-OPEN))))
(check-expect (describe WORM-CLOSED) (string-append (worm-its-name WORM-CLOSED)
               "'s mouth looks " (head-mouth (worm-head WORM-CLOSED))))

(define (describe worm)
  (string-append (worm-its-name worm) "'s mouth looks " (head-mouth (worm-head worm))))

; Defying Gravity

; A Height is a postive real number
; Interpretation: The height of the mountain
(define HEIGHT 200)

; A MoveSpeed can be one of the following positive integers:
; - EasyUp
; - MediumUp
; - HardUp
; Interpretation: The amount of distance the climber will go up the mountain at a certain interval

(define EASY-UP 3)
(define MEDIUM-UP 2)
(define HARD-UP 1)

; A Terrain is a number in the intervals in the following in order:
; - "easy" from [0, 20]
; - "medium" (20, 50]
; - "hard" (50, 70]
; Interpretation: The height of the climber and the difficulty of the section of mountain to climb

; Examples
(define TERRAIN-EASY 50)
(define TERRAIN-MEDIUM 120)
(define TERRAIN-HARD 170)
(define BG-WD 70)
(define HT1 100)
(define HT2 50)
(define HT3 50)
(define BACKGROUND (above (rectangle BG-WD HT3 "solid" "red")
                   (above (rectangle BG-WD HT2 "solid" "yellow")
                          (rectangle BG-WD HT1 "solid" "green"))))

(define CLIMBER-RAD 20)
(define CLIMBER (circle CLIMBER-RAD "solid" "blue"))

; place-terrain: Terrain -> Image
; Takes in the height of the climber and draws the scene

(define (place-terrain terrain)
  (place-image CLIMBER (/ BG-WD 2) terrain BACKGROUND))

(check-expect (place-terrain TERRAIN-EASY)
              (place-image CLIMBER (/ BG-WD 2) TERRAIN-EASY BACKGROUND))
(check-expect (place-terrain TERRAIN-MEDIUM)
              (place-image CLIMBER (/ BG-WD 2) TERRAIN-MEDIUM BACKGROUND))
(check-expect (place-terrain TERRAIN-HARD)
              (place-image CLIMBER (/ BG-WD 2) TERRAIN-HARD BACKGROUND))

; climb-speed -> Terrain -> MoveSpeed
; Determines the climbing speed depending on where the climber is located

(define (climb-speed terrain)
  (cond [(> terrain HT1) EASY-UP]
        [(> terrain HT2) MEDIUM-UP]
        [(> terrain 0) HARD-UP]))

(check-expect (climb-speed 120) EASY-UP)
(check-expect (climb-speed 70) MEDIUM-UP)
(check-expect (climb-speed 10) HARD-UP)

; climb: Terrain KeyEvent -> Terrain
; Moves the climber up the mountain if the keyevent is correct

(define (climb terrain keyEvent)
  (cond [(string=? keyEvent "up") (- terrain (climb-speed terrain))]
        [else (+ terrain (climb-speed terrain))]))

(check-expect (climb TERRAIN-EASY "up") (- TERRAIN-EASY (climb-speed TERRAIN-EASY)))
(check-expect (climb TERRAIN-EASY "down") (+ TERRAIN-EASY (climb-speed TERRAIN-EASY)))

; reached-the-top?: Terrain -> Boolean
; Determines if the climber has reached the top of the mountain

(define (reached-the-top? terrain)
  (<= terrain CLIMBER-RAD))

(check-expect (reached-the-top? CLIMBER-RAD) #true)
(check-expect (reached-the-top? 50) #false)

; main/climb: Terrain -> Terrain
; Creates and runs the climber and the mountain program

(define (main/climb initial-state)
  (big-bang initial-state
    [on-key climb]
    [to-draw place-terrain]
    [stop-when reached-the-top?]))
    