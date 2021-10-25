;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Lab 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
 
(define WIDTH 500)
(define HEIGHT 500)
(define GRAVITY 0.1)
(define BALL-FREQUENCY 14)
(define BG (empty-scene WIDTH HEIGHT))
(define WINNER (overlay (text "Winner!" 20 "blue") BG))
(define LOSER (overlay (text "Loser!" 20 "red") BG))
 
; A ClickGame is a (make-click-game [List-of Ball] Number Number Number)
(define-struct click-game [balls time clicked missed])
; and represents the list of balls on screen, the time passed, the # clicked, and the # missed
 
; A Ball is a (make-ball Posn Posn Number Color)
(define-struct ball [pos vel rad color])
; and represents its position, velocity, radius, and color
 
; A Posn is a (make-posn Number Number)
 
(define posn-10 (make-posn 10 10))
(define posn-2 (make-posn 2 2))
 
(define ball-1 (make-ball posn-10 posn-2 5 "red"))
(define ball-2 (make-ball (make-posn (add1 WIDTH) 0) (make-posn 0 0) 5 "red"))
 
(define game-1 (make-click-game (list ball-1) 10 1 1))
(define game-2 (make-click-game (list ball-2) 1 0 0))
 
; clickgame-temp : ClickGame -> ?
(define (clickgame-temp cg)
  (... (lob-temp (click-game-balls cg))
       (click-game-time cg)
       (click-game-clicked cg)
       (click-game-missed cg)))
 
; lob-temp : [List-of Ball] -> ?
(define (lob-temp lob)
  (... (cond [(empty? lob) ...]
             [(cons? lob) (... (ball-temp (first lob))
                               (lob-temp (rest lob)))])))
 
; ball-temp : Ball -> ?
(define (ball-temp b)
  (... (posn-temp (ball-pos b)) (posn-temp (ball-vel b)) (ball-rad b) (ball-color b)))
 
; posn-temp : Posn -> ?
(define (posn-temp p)
  (... (posn-x p) (posn-y p)))

; modify-balls : [[List-of Ball] -> [List-of Ball]] ClickGame -> ClickGame
; Modify the list of balls
(define (modify-balls balls-modifier cg)
  (make-click-game (balls-modifier (click-game-balls cg))
                   (click-game-time cg)
                   (click-game-clicked cg)
                   (click-game-missed cg)))
(check-expect (modify-balls rest game-1) (make-click-game '() 10 1 1))
 
; modify-time : [Number -> Number] ClickGame -> ClickGame
; Modify time
(define (modify-time time-modifier cg)
  (make-click-game (click-game-balls cg)
                   (time-modifier (click-game-time cg))
                   (click-game-clicked cg)
                   (click-game-missed cg)))
(check-expect (modify-time add1 game-1) (make-click-game (list ball-1) 11 1 1))
 
; modify-clicked : [Number -> Number] ClickGame -> ClickGame
; Modify clicked
(define (modify-clicked clicked-modifier cg)
  (make-click-game (click-game-balls cg)
                   (click-game-time cg)
                   (clicked-modifier (click-game-clicked cg))
                   (click-game-missed cg)))
(check-expect (modify-clicked add1 game-1) (make-click-game (list ball-1) 10 2 1))
 
; modify-missed : [Number -> Number] ClickGame -> ClickGame
; Modify missed
(define (modify-missed missed-modifier cg)
  (make-click-game (click-game-balls cg)
                   (click-game-time cg)
                   (click-game-clicked cg)
                   (missed-modifier (click-game-missed cg))))
(check-expect (modify-missed add1 game-1) (make-click-game (list ball-1) 10 1 2))

; main : Number Number -> Number
; Given the limit on balls that can be missed and hit, play the game
; and produce the time passed
(define (main missed clicked)
  (local [; click-game-over?/main : ClickGame -> Boolean
          ; Is the game over?
          #;(define (click-game-over?/main cg)
              (click-game-over? cg missed clicked))
          ; final-screen/main : ClickGame -> Image
          ; Final screen
          #;(define (final-screen/main cg)
              (final-screen cg missed clicked))]
    (click-game-time (big-bang (make-click-game '() 0 0 0)
                       [on-tick advance-time]
                       ;[on-mouse click]
                       ;[stop-when click-game-over?/main final-screen/main]
                       [to-draw draw]))))

; advance-time : ClickGame -> ClickGame
; Advance the time
(define (advance-time cg)
  (increment-time
   (generate-ball-if-time
    (apply-gravity
     (move-balls
      (remove-offscreen
       (increment-missed-offscreen cg)))))))
(check-expect (advance-time game-1)
              (make-click-game (list (make-ball
                                      (make-posn 12 12)
                                      (make-posn 2 (+ GRAVITY 2))
                                      5
                                      "red")) 11 1 1))
(check-expect (advance-time game-2)
              (make-click-game '() 2 0 1))
 
; increment-time : ClickGame -> ClickGame
; Increase time
(define (increment-time cg)
  (modify-time add1 cg))
(check-expect (increment-time game-1)
              (make-click-game (list ball-1) 11 1 1))
 
; generate-ball-if-time : ClickGame -> ClickGame
; Generate a ball if it is the right time
(define (generate-ball-if-time cg)
  (local [; new-ball : ? -> Ball
          ; Generate a new ball
          (define (new-ball _)
            (make-ball (make-posn (+ (/ WIDTH 5) (* 3/5 (random WIDTH))) 0)
                       (make-posn (* (if (zero? (random 2)) 1 -1) (random 3)) 0.1)
                       (+ 10 (random 10))
                       (make-color (random 256) (random 256) (random 256))))
          ; add-new-ball-if-appropriate : [List-of Ball] -> [List-of Ball]
          ; Add a new ball if it's time
          (define (add-new-ball-if-appropriate lob)
            (if (zero? (modulo (click-game-time cg) BALL-FREQUENCY))
                (cons (new-ball #f) lob)
                lob))]
    (modify-balls add-new-ball-if-appropriate cg)))
(check-expect (generate-ball-if-time game-1) game-1)
(check-random (generate-ball-if-time (make-click-game '() 0 0 0))
              (make-click-game
               (list (make-ball (make-posn (+ (/ WIDTH 5) (* 3/5 (random WIDTH))) 0)
                                (make-posn (* (if (zero? (random 2)) 1 -1) (random 3)) 0.1)
                                (+ 10 (random 10))
                                (make-color (random 256) (random 256) (random 256))))
               0 0 0))
 
; apply-gravity : ClickGame -> ClickGame
; Increase the velocity of the balls due to gravity
(define (apply-gravity cg)
  (local [; change-gravity: [List-of Ball] -> [List-of Ball]
          ; Increasing the velocity of each ball from gravity
          (define (change-gravity lob)
            (map change-ball-gravity lob))
          ; change-ball-gravity: Ball -> Ball
          ; Increases the velocity of a ball with gravity
          (define (change-ball-gravity b)
            (make-ball (ball-pos b)
                       (make-posn (posn-x (ball-vel b)) (+ (posn-y (ball-vel b)) GRAVITY))
                       (ball-rad b) (ball-color b)))]
    (modify-balls change-gravity cg)))
(check-expect (apply-gravity game-1)
              (make-click-game (list (make-ball posn-10 (make-posn 2 (+ 2 GRAVITY)) 5 "red")) 10 1 1))
 
; move-balls : ClickGame -> ClickGame
; Move the balls
(define (move-balls cg)
  (local [; move-all-balls: [List-of Ball] -> [List-of Ball]
          ; Moves all balls in a list of balls
          (define (move-all-balls lob)
            (map move-ball lob))
          ; move-ball: Ball -> Ball
          ; Moves ball
          (define (move-ball b)
            (make-ball (make-posn (+ (posn-x (ball-vel b)) (posn-x (ball-pos b)))
                                  (+ (posn-y (ball-vel b)) (posn-y (ball-pos b))))
                       (ball-vel b) (ball-rad b) (ball-color b)))]
    (modify-balls move-all-balls cg)))
            
(check-expect (move-balls game-1)
              (make-click-game (list (make-ball (make-posn 12 12) posn-2 5 "red")) 10 1 1))
 
; remove-offscreen : ClickGame -> ClickGame
; Remove offscreen balls
#;(define (remove-offscreen cg)
    (local [; offscreen?: Ball -> Boolean
            ; Is the ball off of the screen?
            (define (offscreen? cg)
              (or (< (posn-x (ball-pos ball)) -(ball-rad ball)) 
                  (> (posn-x (ball-pos ball)) (+ WIDTH (ball-rad ball)))
                  (< (posn-y (ball-pos ball)) -(ball-rad ball))
                  (> (posn-y (ball-pos ball)) (+ WIDTH (ball-rad ball)))))]
      (append (filter (λ (b) (not (offscreen? b))) (click-game-lob lob))
              (filter offscreen-vertical? (click-game-lob)))))

(check-expect (remove-offscreen game-1) game-1)
(check-expect (remove-offscreen game-2)
              (make-click-game '() 1 0 0))
 
; increment-missed-offscreen : ClickGame -> ClickGame
; Increment the balls that were missed
#;(define (increment-missed-offscreen cg) ...)
(check-expect (increment-missed-offscreen game-1) game-1)
(check-expect (increment-missed-offscreen game-2)
              (make-click-game (list ball-2) 1 0 1))

; offscreen? : Ball -> Boolean
; Is b offscreen?
(define (offscreen? b)
  (local [; offscreen?/posn : Posn -> Boolean
          ; Is p offscreen?
          (define (offscreen?/posn p)
            (or (< (posn-x p) 0)
                (>= (posn-x p) WIDTH)
                (< (posn-y p) 0)
                (>= (posn-y p) HEIGHT)))]
    (offscreen?/posn (ball-pos b))))
(check-expect (offscreen? ball-1) #f)
(check-expect (offscreen? ball-2) #t)

; remove-offscreen : ClickGame -> ClickGame
; Remove offscreen balls
(define (remove-offscreen cg)
  (local [; remove-offscreen/lob : [List-of Ball] -> [List-of Ball]
          ; Remove offscreen balls in lob
          (define (remove-offscreen/lob lob)
            (filter (λ (b) (not (offscreen? b))) lob))]
    (modify-balls remove-offscreen/lob cg)))
(check-expect (remove-offscreen game-1) game-1)
(check-expect (remove-offscreen game-2)
              (make-click-game '() 1 0 0))
 
; increment-missed-offscreen : ClickGame -> ClickGame
; Increment the balls that were missed
(define (increment-missed-offscreen cg)
  (local [; add-offscreen : Number -> Number
          ; Add the count of the offscreen balls in cg to missed
          (define (add-offscreen missed)
            (foldr (λ (b count)
                     (if (offscreen? b)
                         (add1 count)
                         count))
                   missed
                   (click-game-balls cg)))]
    (modify-missed add-offscreen cg)))
(check-expect (increment-missed-offscreen game-1) game-1)
(check-expect (increment-missed-offscreen game-2)
              (make-click-game (list ball-2) 1 0 1))

;is-clicked?: Ball x y -> Boolean
; Is the given position within the ball?
(define (is-clicked? b x y)
  (local [(define dist (sqrt (+ (sqr (- (posn-x (ball-pos b)) x))
                                (sqr (- (posn-y (ball-pos b)) y)))))]
    (<= dist (ball-rad b))))

(check-expect (is-clicked? ball-1 11 11) #true)
(check-expect (is-clicked? ball-2 200 3) #false)

; draw : ClickGame -> Image
; Draws a ClickGame as an image
(define (draw cg)
  (local [; draw-ball: Ball Image -> Image
          ; Draws a ball on a background
          (define (draw-ball b img)
            (place-image (create-ball-img b) (posn-x (ball-pos b)) (posn-y (ball-pos b)) img))
          ; create-ball-img: Ball -> Image
          ; Draws a circle based on Ball's valuess
          (define (create-ball-img b)
            (circle (ball-rad b) "solid" (ball-color b)))]
    (foldr draw-ball BG (click-game-balls cg))))
