;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Structures) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Structures
;; posn - built in structure
;; 1) constructor make-posn

(make-posn 3 4)

;; 2)Selectors posn-x, posn-y

(posn-x (make-posn 3 4))
(posn-y (make-posn 3 4))

;;3)Predicate posn?

(posn? (make-posn 5 10))
(posn? "hello")

;; These are the default functions that are made with each object/structure


;; -----------------------
;; Define a function, dist-to-0, that computes the distance from a
;; posn to the origin

;; A posn is a (make-posn Nmber Number
;; Interp: A point in a cartesian plane

(define pos1 (make-posn 0 0))
(define posn2 (make-posn 3 4))
(define posn3 (make-posn 0 0)

;; posn-temp : posn -> distance between points
;; List every selector for the structure being used in the function,
;; for the template
(define (posn-temp p)
  (... (posn-x p) ... (posn-y p) ...))

  

  