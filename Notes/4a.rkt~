;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; An NNN is a non-negative number
;; Interp:
(define n1 100)

(define (nnn-temp nnn)
  (... nnn ...))
  
;; speeding-fine: NNN -> NNN
;; Compute the fine (if any) that a driver has to pay if they going _speed_ mph
;; in a narea where the speed limit is _limit_ mph
;; and outputs the fine in dollars.

(check-expect (speeding-fine 60 55) 0)
(check-expect (speeding-fine 65 55) 165)
(check-expect (speeding-fine 75 55) 190)
(check-expect (speeding-fine 88 55) 215)
(check-expect (speeding-fine 95 55) 240)
(check-expect (speeding-fine 106 55) 1000)

(define (speeding-fine speed limit)
  (cond [(< (over speed limit) 10) 0]
        [(< (over speed limit) 20) 165]
        [(< (over speed limit) 30) 190]
        [(< (over speed limit) 40) 215]
        [(< (over speed limit) 50) 240]
        [(>= (over speed limit) 50) 1000]))
        

;; over: NNN NNN -> NNN
;; Determine how much a driver going speed is over the limit (if any)

;; speed 60 65 75 88 95 106
;; limit 55 55 55 55 55 55
;; --------------------------
;;   over 5 10 20 33 40 51

(define (over speed limit)
  (max (- speed limit) 0))