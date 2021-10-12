;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Nat is one of:
; - 0
; - (add1 Nat)
 
(define nat0 0)
(define nat1 (add1 nat0))
(define nat2 (add1 nat1))
 
; nat-temp : Nat -> ?
(define (nat-temp nat)
  (cond
    [(zero? nat) ...]
    [(positive? nat) (... (nat-temp (sub1 nat)) ...)]))

; double : Nat -> Nat
; Double the Nat
(define (double nat)
  (cond
    [(zero? nat) 0]
    [(positive? nat) (add1 (add1 (double (sub1 nat))))]))
 
(check-expect (double nat0) 0)
(check-expect (double nat1) 2)
(check-expect (double nat2) 4)
