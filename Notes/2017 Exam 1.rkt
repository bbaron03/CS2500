;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2017 Exam 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;(define UFO ...)
(define TANK empty-image)
(define MISSILE empty-image)
(define-struct game [ufo tank mis])
; Game is (make-game Posn Posn Posn).
; Game Image -> image
; add the images of the tank and MISSILE to img
(define (render w img)
  (local [(define tank-x (posn-x (game-tank w)))
          (define tank-y (posn-y (game-tank w)))
          (define missile-x (posn-y (game-mis w)))
          (define missile-y (posn-y (game-mis w)))
          (define missile-img (place-image MISSILE missile-x missile-y img))]
  (place-image TANK
               tank-x
               tank-y
               missile-img)))

; adder : [List-of Posn] -> Number
; Computes the sum of all fields of every posn in a list of posns
(check-expect (adder (list (make-posn 0 0))) 0)
(check-expect (adder (list (make-posn -5 -2)
                           (make-posn -6 -3))) -16)
(check-expect (adder (list (make-posn 10 1)
                           (make-posn 4 8))) 23)

(define (adder lop)
  (cond [(empty? lop) 0]
        [(cons? lop) (+ (posn-x (first lop))
                        (posn-y (first lop))
                        (adder (rest lop)))]))

(define-struct world [time-left scores])
(define-struct score [name value])
; A World is (make-world N List-of-scores).
; A List-of-scores is one of:
; -- '()
; -- (cons Score List-of-scores)
; A Score is (make-score String N).
; N: recall that N represents natural numbers.

; update-score : World String -> World
; Increases the value field in the Scores whose
; name field is the same as the given String.
(check-expect (update-score
               (make-world 20 (list (make-score "barry" 3)
                                    (make-score "bonds" 4))) "bonds")
              (make-world 20 (list (make-score "barry" 3) (make-score "bonds" 5))))
(check-expect (update-score
               (make-world 20 (list (make-score "Thomas" 1)
                                    (make-score "The" 1)
                                    (make-score "Train" 3))) "The")
              (make-world 20 (list (make-score "Thomas" 1)
                                    (make-score "The" 2)
                                    (make-score "Train" 3))))

(define (update-score world name)
  (local [; update-score/scores : [List-of Score] String -> [List-of Score]
          ; update-score/scores the score whose name field matches the given String
          (define (update-score/scores los n)
            (cond [(empty? los) '()]
                  [(cons? los) (if (string=? name (score-name (first los)))
                                   (cons (make-score (score-name (first los))
                                                     (add1 (score-value (first los))))
                                         (update-score/scores (rest los) n))
                                   (cons (first los) (update-score/scores (rest los) n)))]))]
    (make-world (world-time-left world) (update-score/scores (world-scores world) name))))

; process : {X,Y} [List-of X] [Y Y -> Boolean] [X -> Y] [X -> X] Y -> [List-of X]
; Creates a list of X where if their Y compared to a given Y returns true in a
; given boolean compare function, a modifier is applied to said X
(check-expect (process (list (make-posn 5 3) (make-posn 4 5))
                       (λ (x1 x2) (> x1 x2)) posn-x
                       (λ (p) (make-posn (- (posn-x p)) (posn-y p)))
                       0) (list (make-posn -5 3) (make-posn -4 5)))
(check-expect (process (list (make-posn 7 3))
                       (λ (x1 x2) (= x1 x2)) posn-y
                       (λ (p) (make-posn 1000 1000))
                       5) (list (make-posn 7 3)))
                         
(define (process lox pred? extractor modifier y)
  (cond [(empty? lox) '()]
        [(cons? lox) (if (pred? (extractor (first lox)) y)
                         (cons (modifier (first lox))
                               (process (rest lox) pred? extractor modifier y))
                         (cons (first lox)
                               (process (rest lox) pred? extractor modifier y)))]))

