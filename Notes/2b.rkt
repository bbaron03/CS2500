;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Import animate and image libraries
(require 2htdp/image)
(require 2htdp/universe)

;; What is the sign of the given number n?
(define (sign n)
  (cond
    [(> n 0) "positive"]
    [(< n 0) "negative"]
    [(= n 0) "zero"]
    )
  )

;; Sign function call
(sign -5)

;; Lets make the sun set, and darken the sky as it sets
(define SUNRAD 30)
(define RECTWD 300)
(define RECTHT 400)
(define SUN (circle SUNRAD  "solid" "yellow"))
(define SKY1 (rectangle RECTWD RECTHT "solid" "lightblue"))
(define SKY2 (rectangle RECTWD RECTHT "solid" "blue"))
(define SKY3 (rectangle RECTWD RECTHT "solid" "black"))
(define X 150)
(define Y0 50)

(define (sunset t)
  (cond
    [(< (+ Y0 t) (- RECTHT SUNRAD)) (place-image SUN X (+ Y0 t) SKY1)]
    [(< (+ Y0 t) (+ RECTHT SUNRAD))  (place-image SUN X (+ Y0 t) SKY2)]
    [(>= (+ Y0 t) (+ RECTHT SUNRAD)) (place-image SUN X (+ Y0 t) SKY3)]
    [(>= (+ Y0 t) (+ RECTHT (* 2 SUNRAD))) (exit)]
))
(animate sunset)