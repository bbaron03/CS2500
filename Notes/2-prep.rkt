;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2-prep) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Converts fahrenheit into celsius c = (f-32) * 5/9
(define (f-to-c f)
  (* (- f 32) 5/9))

;; Function that says hi to the first param and welcome to the second param
(define (greeting p1 p2)
  (string-append "Hello " p1 "." " Welcome " p2 ".")
  
  )

;; Fahrentheit to celsius function call
(f-to-c 32)

;; Greeting call
(greeting "Brendan" "Michael")