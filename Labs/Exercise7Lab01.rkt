;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Exercise7Lab01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 7 Define a function multiple-of-5? that accepts a
; number and determines if it is a multiple 5.
(define (multiple-of-5 n)
  (if (= (modulo n 5) 0) true false)
)

(multiple-of-5 10)
(multiple-of-5 5)
(multiple-of-5 1023)
(multiple-of-5 10000)