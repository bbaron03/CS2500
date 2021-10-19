;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |hw 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; sum-x-coords : [List-of Posn] -> Number
; Sum all the x-coordinates in the list of positions
#;(define (sum-x-coords lop)
(cond
[(empty? lop) 0]
[(cons? lop)
(+ (posn-x (first lop))
(sum-x-coords (rest lop)))]))

(define (sum-x-coords lop)
  (process + posn-x 0 lop))

(check-expect (sum-x-coords empty) 0)
(check-expect (sum-x-coords
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 8)
 
; mult-distances : [List-of Posn] -> Number
; Multiply all the distances from each position to the origin
#;(define (mult-distances lop)
    (cond
      [(empty? lop) 1]
      [(cons? lop)
       (* (distance-to-origin (first lop))
          (mult-distances (rest lop)))]))

(define (mult-distances lop)
  (process * distance-to-origin 1 lop))
 
(check-expect (mult-distances empty) 1)
(check-expect (mult-distances
               (cons (make-posn 3 4)
                     (cons (make-posn 5 12)
                           empty))) 65)
 
; distance-to-origin : Posn -> Number
; Produces the distance from this position to the origin
(define (distance-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))
 
(check-within (distance-to-origin (make-posn 2 2)) (sqrt 8) 1e-6)
(check-expect (distance-to-origin (make-posn 3 4)) 5)

; process: {X,Y} [X Y -> X] [Y -> X] X [List-of Y] -> X
; Performs an operation on every element of a list of y to obtain a single x

(define (process op selector base l)
  (cond [(empty? l) base]
        [(cons? l) (op (selector (first l)) (process op selector base (rest l)))]))

; strings-length: [List-of String] -> Number
; Sums the lengths of all strings in the list
(check-expect (strings-length (list "hi" "im" "tom")) 7)
(check-expect (strings-length (list "")) 0)
(check-expect (strings-length (list "I am a string of length 26")) 26)

(define (strings-length los)
  (process + string-length 0 los))

; biggest-difference : [List-of Posn] -> PosNumber
; Produces the largest difference between a posns’s x and y values in a list of posns
(define (biggest-difference lop)
  (local [; return-larger: Number Number -> PosNumber
          ; Determines the larger of two numbers
          (define (return-larger n1 n2)
            (if (> n1 n2) n1 n2))
          ; distance-x-y : Posn -> PosNumber
          ; Computes the absolute value of the difference between a posn's x and y values
          (define (distance-x-y p)
            (abs (- (posn-x p) (posn-y p))))]
    (process return-larger distance-x-y 0 lop)))

(check-expect (biggest-difference (list (make-posn 3 5)
                                        (make-posn 11 15)
                                        (make-posn 1 15))) 14)
(check-expect (biggest-difference (list (make-posn 5 5)
                                        (make-posn -28 15))) 43)
(check-expect (biggest-difference (list (make-posn 25 20)
                                        (make-posn 30 1)
                                        (make-posn 1 -5))) 29)
(check-expect (biggest-difference '()) 0)

; A [NEL-of X] is a [List-of X] where [List-of X] has at least one element of X.
; An [NEL-of X] is a non-empty list of a given data type.

; Examples
(define nel-1 (list 1 5 9))
(define nel-2 (list "Hi" "Im" "BOb"))
(define nel-3 (list #true #false #false #true))

; nel-temp: [NEL-of X] -> ?
(define (nel-temp nel)
  (cond [(empty? nel) ...]
       [(cons? nel) (... (first nel) ...
                    ... (nel-temp (rest nel)) ...)]))

; earliest : [NEL-of String] [String String -> Boolean] -> String
; Outputs the string that comes earliest in the non-empty list according to the given function.
(check-expect (earliest nel-2 string<?) "BOb")
(check-expect (earliest (list "b" "c" "a" "x" "y") string<?) "a")
(check-expect (earliest (list "b" "c" "a" "x" "y") string>?) "y")

(define (earliest nel compare)
  (cond [(empty? (rest nel)) (first nel)]
        [(cons? (rest nel)) (if (compare (first nel) (earliest (rest nel) compare))
                                (first nel)
                                (earliest (rest nel) compare))]))

; earliest-lex : [NEL-of String] -> String
; Produces the string that comes earliest lexicographically
(check-expect (earliest-lex (list "b" "A" "c" "a" "x" "y")) "A")
(check-expect (earliest-lex (list "b" "c" "x" "y" "F")) "F")

(define (earliest-lex nel)
  (earliest nel string<?))

; earliest-lastlex : [NEL-of String] -> String
; Produces the string that comes latest lexicographically
(check-expect (earliest-lastlex (list "b" "c" "a" "g" "m" "o" "p" "l")) "p")
(check-expect (earliest-lastlex (list "x" "z" "y" "A" "b" "Z")) "z")

(define (earliest-lastlex nel)
  (earliest nel string>?))

; last-string: [NEL-of String] -> String
; Produces the last string in the non empty list.
(check-expect (last-string (list "To" "Be" "Or" "Not" "To" "Last")) "Last")
(check-expect (last-string (list "rock" "and" "roll")) "roll")
(check-expect (last-string (list "I" "am" "the" "champion")) "champion")
(check-expect (last-string (list "I")) "I")

(define (last-string nel)
  (earliest nel rest-empty?))

; rest-empty? : String [List-of String] -> Boolean
; Is the given list empty?
(check-expect (rest-empty? "String" (list "String" "Non-empty")) #f)
(check-expect (rest-empty? "Hello!" '()) #true)

(define (rest-empty? s los)
  (empty? los))

; two-copies : [List-of String] -> [List-of String]
; Given a list of strings, returns the same list but with two copies
; (next to each other in the list, as separate strings) of all strings with an even string-length.
(check-expect (two-copies (list "hi" "im" "brendan")) (list "hiim" "hiim"))
(check-expect (two-copies (list "even" "odd" "test")) (list "eventest" "eventest"))
(check-expect (two-copies (list "")) (list "" ""))

(define (two-copies los)
  (local [; string-length-even?: String -> Boolean
          ; Is the given string's length even?
          (define (string-length-even? s)
            (even? (string-length s)))
          (define filtered-los (filter string-length-even? los))

          ; concat : [List-of String] -> String
          ; Joins all strings in a list of strings
          (define (concat los)
             (foldr string-append "" los))

          ; clone: String -> [List-of String]
          ; Clones each element of the list
          (define (clone s b)
            (cons s (cons s b)))
          ]
  (foldr clone '() (list (concat filtered-los)))))

(define-struct cup [oz color material])
 
; A Cup is a (make-cup NonNegNumber String String)
; and represents a cup's capacity in fluid ounces, color, and material
 
(define CUP1 (make-cup 10 "brown" "wood"))
(define CUP2 (make-cup 8 "brown" "ceramic"))
(define CUP3 (make-cup 10 "red" "plastic"))
(define CUP4 (make-cup 6 "clear" "plastic"))
 
(define CUPS
  (cons CUP1
        (cons CUP2
              (cons CUP3
                    (cons CUP4 empty)))))

; create-grouping: {X, Y} [List-of X] [X -> Y] [X X -> Boolean] -> [List-of X]
; takes in 3 arguments - a list of elements, a key extractor,
; and a key equivalence relation - and produces a grouping.


