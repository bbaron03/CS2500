;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
; Finger exercises
; Exercise 1 What is a well-matched data representation of

; the speed of a car : an integer
; a name of a person: string
; the state of a light switch: boolean
; a full street address of a person: string
; the velocity of a boat (on a flat 2-dimensional lake): float
; the state of a working street light: boolean
; the (regular) semester grades given to students: float or string
; a boarding pass for a flight: object
; an email address: string
; a location of a point in a virtual 3-dimension world: set

; Exercise 34. Design the function string-first,
; which extracts the first character from a non-empty string. Don’t worry
; about empty strings. 

(define (string-first word)
  (substring word 0 1)
)
; Exercise 35. Design the function string-last, which extracts the last
; character from a non-empty string. 
(define (string-last word)
  (substring word (- (string-length word) 1) (string-length word))
)
; Exercise 36. Design the function image-area, which
; counts the number of pixels in a given image. 
(define (image-area img)
  (* (image-height img) (image-width img))
)
; Exercise 37. Design the function string-rest, which produces
; a string like the given one with the first character removed. 
(define (string-rest word)
  (substring word 1 (string-length word))
)
; Exercise 38. Design the function string-remove-last, which produces a
; string like the given one with the last character removed.
(define (string-remove-last word)
  (substring word 0 (- (string-length word) 1))
)