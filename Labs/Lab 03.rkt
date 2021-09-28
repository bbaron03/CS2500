;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 03|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A PRS (PetRockStorage) is one of:
; - "King Paimon"
; - (make-bag String Number PRS)
(define-struct bag [color size contents])
; and represents the pet rock
; or a bag containing it (and possibly other bags)
;   with a specific color and size in cubic centimeters.

; Examples
(define prs-0 "King Paimon")
(define prs-1 (make-bag "red" 10 prs-0))
(define prs-2 (make-bag "blue" 11 prs-1))
(define prs-3 (make-bag "green" 200 prs-2))
 
; prs-temp : PRS -> ???
(define (prs-temp prs)
  (cond
    [(string? prs) ...]
    [(bag? prs)
     (... (bag-color prs)
          (bag-size prs)
          (prs-temp (bag-contents prs)))]))

; largest-bag-size : PRS -> Number
; determines the size of the largest bag in a PRS

(check-expect (largest-bag-size prs-0) 0)
(check-expect (largest-bag-size prs-1) 10)
(check-expect (largest-bag-size prs-2) 11)
(check-expect (largest-bag-size prs-3) 200)

(define (largest-bag-size prs)
  (cond
    [(string? prs) 0]
    [(bag? prs)
     (max (bag-size prs)
          (largest-bag-size (bag-contents prs)))]))

; color-in-bag?: PRS String -> Boolean
; Determines whether a certain color is in a given PRS

(check-expect (color-in-bag? prs-0 "blue") #false)
(check-expect (color-in-bag? prs-1 "red") #true)
(check-expect (color-in-bag? prs-2 "red") #true)
(check-expect (color-in-bag? prs-3 "yellow") #false)

(define (color-in-bag? prs color)
  (cond
    [(string? prs) #false]
    [(bag? prs) (or (string=? (bag-color prs) color)
                (color-in-bag? (bag-contents prs) color))]))

; increase-bag-size: PRS -> PRS
; Increases all of the bag-sizes of a prs by one
(define incremented-prs-1 (make-bag (bag-color prs-1)
                                    (add1 (bag-size prs-1))
                                       prs-0))
(define incremented-prs-2 (make-bag (bag-color prs-2)
                                    (add1 (bag-size prs-2))
                                    incremented-prs-1))
(define incremented-prs-3 (make-bag (bag-color prs-3)
                                    (add1 (bag-size prs-3))
                                    incremented-prs-2))

(check-expect (increase-bag-size prs-0) prs-0)
(check-expect (increase-bag-size prs-1) incremented-prs-1)
(check-expect (increase-bag-size prs-2) incremented-prs-2)
(check-expect (increase-bag-size prs-3) incremented-prs-3)
                                                  
(define (increase-bag-size prs)
  (cond
    [(string? prs) prs]
    [(bag? prs) (make-bag (bag-color prs)
                (add1 (bag-size prs))
                (increase-bag-size (bag-contents prs)))]))

; remove-bigger: PRS Number -> PRS
; Removes all bags bigger than the given size

(check-expect (remove-bigger prs-1 5) prs-0)
(check-expect (remove-bigger prs-2 10) prs-1)
(check-expect (remove-bigger prs-3 100) prs-2)

(define (remove-bigger prs size)
  (cond
    [(string? prs) prs]
    [(bag? prs)
     (if (<= (bag-size prs) size)
         (make-bag (bag-color prs) (bag-size prs) (remove-bigger (bag-contents prs) size))
         (remove-bigger (bag-contents prs) size))]))

; well-stored?: PRS -> Boolean
(check-expect (well-stored? prs-0) #false)
(check-expect (well-stored? prs-1) #true)
(check-expect (well-stored? prs-2) #true)
(check-expect (well-stored? prs-3) #true)

(define (well-stored? PRS -> Boolean)
  (cond [(string? prs) #true]
        [(bag? prs)
         (and (bag-size 

; A MidpointGame is one of:
; - (make-pre-click Posn Posn)
; - (make-post-click Posn Posn Posn)
(define-struct pre-click [p1 p2])
(define-struct post-click [p1 p2 p-click])
; and represents the two points to be clicked between
; as well as where the player clicked (once they do)
 
; A Posn is a (make-posn Number Number)
(define posn-0 (make-posn 0 0))
(define posn-10 (make-posn 10 10))
(define posn-click (make-posn 6 6))
 
(define mpg-pre (make-pre-click posn-0 posn-10))
(define mpg-post (make-post-click posn-0 posn-10 posn-click))
 
; mg-temp : MidpointGame -> ?
(define (mg-temp mg)
  (cond [(pre-click? mg) (... (posn-temp (pre-click-p1 mg))
                              (posn-temp (pre-click-p2 mg)))]
        [(post-click? mg) (... (posn-temp (post-click-p1 mg))
                               (posn-temp (post-click-p2 mg))
                               (posn-temp (post-click-pclick mg)))]))
 
; posn-temp : Posn -> ?
(define (posn-temp p)
  (... (posn-x p) (posn-y p)))

; player-guess : MidpointGame Integer Integer MouseEvent -> MidpointGame
; Handles a user click:
;   if the game is pre-click, transition to a post-click game
;   if the game is post-click, do nothing
(check-expect (player-guess mpg-pre 6 6 "button-down") mpg-post)
(check-expect (player-guess mpg-pre 5 5 "button-up") mpg-pre)
(check-expect (player-guess mpg-post 4 4 "button-down") mpg-post)

(define (player-guess mg x y me)
  (cond [(pre-click? mg)
         (if (string=? me "button-down")
             (make-post-click (pre-click-p1 mg)
                              (pre-click-p2 mg)
                              (make-posn x y))
             mg)]
        [(post-click? mg) mg]))

; midpoint: Posn Posn -> Posn
; Computes the midpoint of two posns
(check-expect (midpoint posn-0 posn-10) (make-posn 5 5))
(check-expect (midpoint posn-0 posn-click) (make-posn 3 3))

(define (midpoint posn-1 posn-2)
  (make-posn (/ (+ (posn-x posn-1) (posn-x posn-2)) 2)
             (/ (+ (posn-y posn-1) (posn-y posn-2)) 2)))