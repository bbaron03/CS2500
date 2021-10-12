;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname HW5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An LoN (List of Numbers) is one of:
; - '()
; - (cons Number LoN)

; Examples
(define lon-empty '())
(define lon1 (list 1))
(define lon2 (list 2 3 4))
(define lon3 (list 5 6 7))
(define lon4 (list 8 9 1 5))

; lon-temp: LoN -> ?
#;(define (lon-temp lon)
    (cond [(empty? lon) ...]
          [(cons? lon) ... (first lon) ... (rest lon) ...]))

; interleave: LoN LoN -> LoN
; Produces a list of the two LoN's items, alternating from each list.
(check-expect (interleave lon-empty lon1) lon1)
(check-expect (interleave lon1 lon2) (list 1 2 3 4))
(check-expect (interleave lon2 lon3) (list 2 5 3 6 4 7))
(check-expect (interleave lon3 lon4) (list 5 8 6 9 7 1 5))

(define (interleave lon1 lon2)
  (cond [(empty? lon1) (if (empty? lon2)
                           '()
                           (cons (first lon2) (interleave lon1 (rest lon2))))]
        [(cons? lon1) (cons (first lon1) (interleave lon2 (rest lon1)))]))

; powerlist : LoN -> [List LoN]
; Creates a list of all possible sublists of a LoN
(define powerlist-lon2 (list '() (list 2) (list 3) (list 4)
                             (list 2 3) (list 2 4) (list 3 4)
                             (list 2 3 4)))
;(check-expect (powerlist lon-empty) '())
;(check-expect (powerlist lon1) (list '() (list 1)))
;(check-expect (powerlist lon2) powerlist-lon2)


; intersection: [List-of LoN] -> LoN
; Determines the intersection, or equivalent elements, of all the LoNs in a [List-of LoN]

;;;;;;;;;;;;BOX OUT;;;;;;;;;;;;;;;;;;;;;;
(define win-constant .75)
(define BOARD-HEIGHT 200)
(define BOARD-WIDTH 400)

; A Posn is a (make-posn x y), where
; x is a Number and
; y is a Number.
 
; posn-temp : Posn -> ???
(define (posn-temp p)
  (... (posn-x p) ... (posn-y p) ...))

; A Ball is a Posn
; A Ball represents a ball on the board where the Posn
; is the ball's location.
; Examples:
(define ball-1 (make-posn 0 0))
(define ball-2 (make-posn BOARD-WIDTH BOARD-HEIGHT))
(define ball-3 (make-posn 55 46))
(define ball-4 (make-posn 200 150))
(define ball-5 (make-posn BOARD-WIDTH 95))

; ball-temp: Ball -> ?
(define (ball-temp ball)
  (... (posn-temp ball) ...))

; A ListOfBalls is a [List-of Ball]
; A LoB is the collection of Balls
; Examples:
(define lob-1 (list ball-1))
(define lob-2 (list ball-1 ball-2))
(define lob-3 (list ball-1 ball-2 ball-3))
(define lob-4 (list ball-1 ball-2 ball-3 ball-4))
(define lob-5 (list ball-1 ball-2 ball-3 ball-4 ball-5))

; lob-temp: LoB -> ?
(define (lob-temp lob)
  (... cond [(empty? lob) ...]
       [(cons? lob) ... (ball-temp (first lob)) ...
                    ... (lob-temp (rest lob)) ...]))

(define-struct region [top-left bot-right balls-inside])
; A Region is a (make-region Posn Posn LoB)
; A (make-region tl br lb) represents a rectangular region
; where the top left corner is at tl and the bottom right
; corner is locate at br. Each corner of the region must be
; a point such that for each corner, 0 <= x <= BOARD-WIDTH and 0 <= y <= BOARD-HEIGHT.
; Also, tl-x < br-x, tl-y < br-y. Lb is the balls inside the given region as well.
; Examples:
(define region-1 (make-region (make-posn 0 0) (make-posn BOARD-WIDTH BOARD-HEIGHT) lob-5))
(define region-2 (make-region (make-posn 50 45) (make-posn 75 50) (list ball-3)))
(define region-3 (make-region (make-posn 0 (- BOARD-HEIGHT 1)) (make-posn 1 BOARD-HEIGHT) '()))
(define region-4 (make-region (make-posn 33 27) (make-posn 300 194) (list ball-3 ball-4)))

; region-temp: Region -> ?
(define (region-temp region)
  (... (posn-temp (top-left region)) ... (posn-temp (bot-right region))))

; A ListOfRegions is a [List-of Region]
; A LoR is a list of each region of the board
; Examples:
(define lor-0 '())
(define lor-1 (list region-1))
(define lor-2 (list region-1 region-2))
(define lor-3 (list region-1 region-2 region-3))
(define lor-4 (list region-1 region-2 region-3 region-4))

; lor-temp: LoR -> ?
(define (lor-temp lor)
  (... cond [(empty? lor) ...]
       [(cons? lor) ... (region-temp (first lor)) ...
                    ... (lor-temp (rest lor)) ...]))

(define-struct wall [is-vertical? location])
; A Wall is a (make-wall Boolean Number)
; A (make-wall iv loc) is a wall that is
; vertical if iv is #true, and horizontal when
; iv is #false. If the line is vertical, then loc is the
; walls x position and if the line is horizontal, loc is
; the walls y position.
; Examples:
(define wall-1 (make-wall #true 124))
(define wall-2 (make-wall #false 21))
(define wall-3 (make-wall #true 390))
(define wall-4 (make-wall #false 1))
(define wall-5 (make-wall #true 249))

; wall-temp: Wall -> ?
(define (wall-temp wall)
  (... (direction wall) ... (location wall) ...))

; A MousePosn is a Posn
; A MousePosn is the location of the mouse cursor
; Examples:
(define mouseposn1 (make-posn 0 0))
(define mouseposn2 (make-posn 76 175))
(define mouseposn3 (make-posn 353 10))
  
; mouseposn-temp: MousePosn -> ?
(define (mouseposn-temp mouseposn)
  (... (posn-temp mouseposn) ...))

(define-struct board-state [lor lob wall mouseposn])
; A BoardState is a (make-board-state LoR LoB Wall MousePosn)
; A (make-board-state lr lb w mp) is a Box Out game state where there are
; regions lor, with balls lob, a wall w that is currently being made, and a mouse
; position of mp
; Examples:

; board-state-temp: BoardState -> ?
(define (board-state-temp board-state)
  (... (lor-temp (board-state-lor board-state)) ...
       ... (lob-temp (board-state-lob board-state)) ...
       ... (wall-temp (board-state-wall board-state)) ...
       ... (mouseposn-temp (board-state-mouseposn board-state)) ...))

; board->image : BoardState -> Image
; Renders the BoardState into an image
(define (board->image boardstate))


; draw-regions: LoR -> Image
; Draws the regions to the board

; draw-balls: LoB -> Image
; Draws the balls to the board

; draw-mouseposn: MousePosn Wall -> Image
; Draws the cursor icon to the board

; toggle-wall-dir: Wall -> Wall
; Inverts the wall direction of a given wall

; level-won?: BoardState -> Boolean
; Determines if the game has beenn won by the player

; total-area-of-regions: LoR -> Number
;

; area-region: Region -> Number
;

; box-out: BoardState -> BoardState
;