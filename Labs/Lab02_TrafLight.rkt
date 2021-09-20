;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lab02_TrafLight) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; a TrafficLight is one of:
;; -"red"
;; -"green"
;; -"yellow"
;; Interpretation: represents the color of a traffic light.

(define TL-RADIUS 40)

; examples
(define TL-RED "red")
(define TL-GREEN "green")
(define TL-YELLOW "yellow")

; tl-temp: TrafficLight -> ?
(define (tl-temp tl)
  (cond [(string=? tl TL-RED) ...]
        [(string=? tl TL-YELLOW) ...]
        [(string=? tl TL-GREEN) ...]))

; loop-light: TrafficLight -> TrafficLight
; Cycles a traffic light between red, yellow, and green
(define (loop-light initial-state)
  (big-bang initial-state
    [on-tick next-light 1]
    [to-draw draw-light]))

; next-light : TrafficLight -> TrafficLight
; Determine the next state of TrafficLight in the normal order
(define (next-light tl)
  (cond [(string=? tl TL-RED) TL-GREEN]
        [(string=? tl TL-YELLOW) TL-RED]
        [(string=? tl TL-GREEN) TL-YELLOW]))

(check-expect (next-light TL-RED) TL-GREEN)
(check-expect (next-light TL-YELLOW) TL-RED)
(check-expect (next-light TL-GREEN) TL-YELLOW)


; draw-light: TrafficLight -> Image
; Draws a circle of the TrafficLight's current color
#;(define (draw-light tl) 
    (circle TL-RADIUS "solid" tl))
; The above function also works just as well

(define (draw-light tl)
  (cond [(string=? tl TL-RED) (circle TL-RADIUS "solid" "red")]
        [(string=? tl TL-YELLOW) (circle TL-RADIUS "solid" "yellow")]
        [(string=? tl TL-GREEN) (circle TL-RADIUS "solid" "green")]))

(check-expect (draw-light TL-RED) (circle TL-RADIUS "solid" "red"))
(check-expect (draw-light TL-GREEN) (circle TL-RADIUS "solid" "green"))
(check-expect (draw-light TL-YELLOW) (circle TL-RADIUS "solid" "yellow"))

