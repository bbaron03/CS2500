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

; process: {X,Y} [Y Y -> Y] [X -> Y] Y [List-of X] -> Y
; Does a computation for each X of a list to get aY, then performs an operation between
; the Y and the rest of the list
(check-expect (process + posn-y 0 (list (make-posn 3 4)
                                        (make-posn 7 12)
                                        (make-posn 5 -5)
                                        (make-posn 15 0))) 11)
(check-expect (process max posn-x 0 '()) 0)
                               
(define (process op modifier base l)
  (cond [(empty? l) base]
        [(cons? l) (op (modifier (first l)) (process op modifier base (rest l)))]))

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
(check-expect (earliest-lastlex (list "w")) "w")

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
(check-expect (two-copies (list "hi" "im" "brendan")) (list "hi" "hi" "im" "im" "brendan"))
(check-expect (two-copies (list "even" "odd" "test")) (list "even" "even" "odd" "test" "test"))
(check-expect (two-copies (list "")) (list "" ""))

(define (two-copies los)
  (local [; string-length-even?: String -> Boolean
          ; Is the given string's length even?
          (define (string-length-even? s)
            (even? (string-length s)))

          ; clone-if-even: String -> [List-of String]
          ; Clones each string of the list that has an even string length
          (define (clone s b)
            (if (string-length-even? s)
                (cons s (cons s b))
                (cons s b)))]
    (foldr clone '() los)))

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

; An LoX is a [List-of X]
; An lox is a list that holds elements of type x
; Examples

(define lox1 (list 1 2 3))
(define lox2 (list CUP1 CUP2 CUP3))
(define lox3 (list #false #true))
(define lox4 (list "string1" "hello" "world!"))

; lox-temp : LoX -> ?
(define (lox-temp lox)
  (cond [(empty? lox) ...]
        [(cons? lox) (... (first lox) ...
                          ... (lox-temp (rest lox)) ...)]))

(define-struct grouping [key lox])
; A Grouping is a (make-grouping Y [List-of X])
; A (make-grouping k lox) is a group where each element X
; of the lox have the same attribute where [X -> Y] returns a Y that is equivalent to k
; Examples:
(define grouping1 (make-grouping "brown" (list CUP1 CUP2)))
(define grouping2 (make-grouping "red" (list CUP3)))
(define grouping3 (make-grouping "ceramic" (list CUP2)))
(define grouping4 (make-grouping "plastic" (list CUP3 CUP4)))
(define grouping5 (make-grouping 10 (list CUP1 CUP3)))
(define grouping6 (make-grouping 6 (list CUP4)))

; grouping-temp: Grouping -> ?
(define (grouping-temp grouping)
  (... (grouping-key grouping) ...
       ... (lox-temp (grouping-lox grouping)) ...))

; create-grouping: {X, Y} [List-of X] [X -> Y] [Y Y -> Boolean] -> [List-of Grouping]
; takes in 3 arguments - a list of elements, a key extractor,
; and a key equivalence relation - and produces a grouping.
(check-expect (create-grouping CUPS cup-color string=?)
              (list (make-grouping "brown" (list CUP1 CUP2))
                    (make-grouping "red" (list CUP3))
                    (make-grouping "clear" (list CUP4))))
(check-expect (create-grouping CUPS cup-material string=?)
              (list (make-grouping "wood" (list CUP1))
                    (make-grouping "ceramic" (list CUP2))
                    (make-grouping "plastic" (list CUP3 CUP4))))
(check-expect (create-grouping CUPS cup-oz =)
              (list (make-grouping 8 (list CUP2))
                    (make-grouping 10 (list CUP1 CUP3))
                    (make-grouping 6 (list CUP4))))

(define (create-grouping lox key-extractor key-eq?)
  (local [(define key-list (form-list-of-keys lox key-extractor key-eq?))

          ; make-new-grouping : {Y} Y -> Grouping
          ; Makes a new Grouping of from an [List-of X] that have key Y
          (define (make-new-grouping key)
            (make-grouping key (form-list-by-key lox key-eq? key-extractor key)))]
    (map make-new-grouping key-list)))

; contains? : [List-of X] X [X X -> Boolean] -> Boolean
; Does a list contain X?
(check-expect (contains? (list 1 2 3) 3 =) #true)
(check-expect (contains? (list "Hi" "Im" "Robocop") "Hooray!" string=?) #false)
(check-expect (contains? (list "brown" "red" "clear") "clear" string=?) #true)

(define (contains? lox x key-eq?)
  (local [; key-eq?/x : X -> Boolean
          ; Is a given X equal to another X?
          (define (key-eq?/x newx)
            (key-eq? x newx))]
    (ormap key-eq?/x lox)))

; form-list-of-keys: {X Y} [List-of X] [X -> Y] [Y Y -> Boolean] -> [List-of Y]
; Forms a list of Y from a given list of X
(check-expect (form-list-of-keys CUPS cup-color string=?)
              (list "brown" "red" "clear"))
(check-expect (form-list-of-keys CUPS cup-material string=?)
              (list "wood" "ceramic" "plastic"))
(check-expect (form-list-of-keys CUPS cup-oz =)
              (list 8 10 6))

(define (form-list-of-keys lox key-extractor key-eq?)
  (local [; cons-key: X [List-of X] [Y Y -> Boolean] -> [List-of X]
          ; Adds X to a list if X is not already in the list
          (define (cons-key x rest)
            (if (not (contains? rest (key-extractor x) key-eq?))
                (cons (key-extractor x) rest)
                rest))]
    (foldr cons-key '() lox)))

; form-list-by-key: {X Y} [List-of X] [Y Y -> Boolean] [X -> Y] Y-> [List-of X]
; Forms a list of X where the extracted key from x equals
; the key given to the function
(check-expect (form-list-by-key CUPS string=?
                                cup-color "brown") (list CUP1 CUP2))
(check-expect (form-list-by-key CUPS string=?
                                cup-material "plastic") (list CUP3 CUP4))
(check-expect (form-list-by-key CUPS =
                                cup-oz 6) (list CUP4))
(check-expect (form-list-by-key (list (make-posn 3 0) (make-posn 5 3)) =
                                posn-x 10) '())
(check-expect (form-list-by-key (list (make-posn 3 0) (make-posn 5 3)) =
                                posn-x 5) (list (make-posn 5 3)))

(define (form-list-by-key lox key-eq? key-extractor key)
  (local [; equals-key?: X -> Boolean
          ; Does X equal the given key?
          (define (equals-key? x)
            (key-eq? (key-extractor x) key))]
    (filter equals-key? lox))) 

; key-in-groupings? : {X Y} Y [Y Y -> Boolean] [List-of Grouping] -> Boolean
; Is a y already used for another grouping in a list of groupings?
(check-expect (key-in-groupings? "brown" string=? (list grouping1 grouping2)) #t)
(check-expect (key-in-groupings? 10 = '()) #f)
(check-expect (key-in-groupings? 6 = (list grouping5 grouping6)) #t)
(check-expect (key-in-groupings? "non-valid key" string=?
                                 (list grouping1 grouping2 grouping3 grouping4)) #f)

(define (key-in-groupings? key key-eq? log)
  (cond [(empty? log) #false]
        [(cons? log) (or (key-eq? key (grouping-key (first log)))
                         (key-in-groupings? key key-eq? (rest log)))]))

;; A [Keyed X] is a (list Number X)
;; and represents an X and its extracted "key" 

;; A Movie is a (make-movie String Number)
(define-struct movie [title runtime])
;; and represents a movie's title and runtime in minutes

;; sort-by-title-length : [List-of Movie] -> [List-of Movie]
;; Sort the movies by their title's length (ascending)
#;(define (sort-by-title-length lom)
    (local
      [;; insert-by-title-length : [Keyed Movie] [List-of [Keyed Movie]] -> [List-of [Keyed Movie]]
       ;; Find the correct spot for the title length
       (define (insert-by-title-length nm lonm)
         (cond [(empty? lonm) (list nm)]
               [(cons? lonm)
                (if (<= (first nm) (first (first lonm)))
                    (cons nm lonm)
                    (cons (first lonm) (insert-by-title-length nm (rest lonm))))]))]
      (map second
           (foldr insert-by-title-length
                  '()
                  (map list (map (compose string-length movie-title) lom) lom)))))

(define (sort-by-title-length lom)
  (sort-by lom (compose string-length movie-title)))
(check-expect (sort-by-title-length
               (list (make-movie "Sorry To Bother You" 111)
                     (make-movie "Hereditary" 127)
                     (make-movie "Annihilation" 120)
                     (make-movie "Blindspotting" 96)
                     (make-movie "You Were Never Really Here" 95)))
              (list
               (make-movie "Hereditary" 127)
               (make-movie "Annihilation" 120)
               (make-movie "Blindspotting" 96)
               (make-movie "Sorry To Bother You" 111)
               (make-movie "You Were Never Really Here" 95)))

;; An [NEList-of X] (Non-Empty List of X) is one of:
;; - (cons X '()))
;; - (cons X [NEList-of X])

;; sort-by-biggest : [List-of [NEList-of Number]] -> [List-of [NEList-of Number]]
;; Sort the lists by their biggest element (ascending)
#;(define (sort-by-biggest lonelon)
    (local [;; A KNELoN is a [Keyed [NEList-of Number]]]
            ;; insert-by-biggest : KNELoN [List-of KNELoN] -> [List-of KNELoN]
            ;; Find the correct spot for the biggest number
            (define (insert-by-biggest nnelon lonnelon)
              (cond [(empty? lonnelon) (list nnelon)]
                    [(cons? lonnelon)
                     (if (<= (first nnelon) (first (first lonnelon)))
                         (cons nnelon lonnelon)
                         (cons (first lonnelon) (insert-by-biggest nnelon (rest lonnelon))))]))
            ;; biggest : [NEList-of Number] -> Number
            ;; Find the biggest number in the non-empty list of numbers
            (define (biggest nelon)
              (foldr max (first nelon) (rest nelon)))]
      (map second (foldr insert-by-biggest '() (map list (map biggest lonelon) lonelon)))))

(define (sort-by-biggest lonelon)
  (local [;; biggest : [NEList-of Number] -> Number
          ;; Find the biggest number in the non-empty list of numbers
          (define (biggest nelon)
            (foldr max (first nelon) (rest nelon)))]
    (sort-by lonelon biggest)))

(check-expect (sort-by-biggest (list (list 6) (list 1 2 3) (list 5 6) (list 23)))
              (list (list 1 2 3) (list 6) (list 5 6) (list 23)))

; sort-by: {X} [List-of X] [(List-of X) -> Number] -> [List-of X]
; Sorts a list by the Number determined from the given funciton in ascending order
(check-expect (sort-by (list (list "strings" "loveem") (list "soo much" "they" "are")
                             (list "great" "I" "mean" "it")) (compose string-length last-string))
              (list (list "great" "I" "mean" "it")
                    (list "soo much" "they" "are")
                    (list "strings" "loveem")))

(define (sort-by lox mapping-compare)
  (local [; insert-by: {X} X [List-of X] -> [List-of X]
          ; Finds the correct spot for X based on the first element of x
          (define (insert-by x lox)
            (cond [(empty? lox) (list x)]
                  [(cons? lox)
                   (if (<= (first x) (first (first lox)))
                       (cons x lox)
                       (cons (first lox) (insert-by x (rest lox))))]))]
    (map second
         (foldr insert-by '()
                (map list (map mapping-compare lox) lox)))))