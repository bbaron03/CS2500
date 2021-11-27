;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname FingerExercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)

(define NP (make-no-parent))

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; count-persons : FT -> Natural
; Counts the children in a Family Tree

(define (count-persons ft)
  (cond [(no-parent? ft) 0]
        [(child? ft) (+ 1 (count-persons (child-father ft))
                        (count-persons (child-mother ft)))]))

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)
(check-expect (count-persons Eva) 3)
(check-expect (count-persons NP) 0)

; average-age : FT Natural -> Number
; Computes the average age of all children in a family tree

(define (average-age ft year)
  (local [; sum-ages : FT -> Number
          ; Adds all the ages of every child in a FT
          (define (sum-ages ft)
            (cond [(no-parent? ft) 0]
                  [(child? ft) (+ (- year (child-date ft))
                                  (sum-ages (child-father ft))
                                  (sum-ages (child-mother ft)))]))]
  (cond [(zero? (count-persons ft)) 0]
        [(positive? (count-persons ft)) (/ (sum-ages ft) (count-persons ft))])))

(check-expect (average-age NP 2010) 0)
(check-expect (average-age Carl 2000) 74)
(check-expect (average-age Eva 2000) 61)
(check-expect (average-age Gustav 1988) 33.8)

; eye-colors: FT -> [List-of String]
; Creates a list of all eye-colors in a family tree

(define (eye-colors ft)
  (local [; get-eye-colors : FT -> [List-of String]
          ; Gets the eye colors of all children in a family tree
          (define (get-eye-colors ft)
            (cond [(no-parent? ft) '()]
                  [(child? ft) (append (list (child-eyes ft)) (eye-colors (child-father ft))
                                       (eye-colors (child-mother ft)))]))
          ; remove-duplicates [List-of String] -> [List-of String]
          ; Removes duplicate strings in a list of strings
          (define (remove-duplicates los)
            (foldr (λ (s r) (if (ormap (λ (s2) (string=? s s2)) r) r (cons s r))) '() los))]
    (remove-duplicates (get-eye-colors ft))))
        
(check-expect (eye-colors NP) '())
(check-expect (eye-colors Carl) (list "green"))
(check-expect (eye-colors Adam) (list "hazel" "green"))
(check-expect (eye-colors Gustav) (list "brown" "pink" "blue" "green"))

; An FF (short for family forest) is one of: 
; – '()
; – (cons FT FF)
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; blue-eyed-child-in-forest? : FF -> Boolean
; Does a FamilyForest contain a child with blue eyes?

(define (blue-eyed-child-in-forest? ff)
  (ormap (λ (c) (string=? "blue" (child-eyes c))) ff))

(check-expect (blue-eyed-child-in-forest? '()) #f)
(check-expect (blue-eyed-child-in-forest? ff1) #f)
(check-expect (blue-eyed-child-in-forest? ff2) #t)
(check-expect (blue-eyed-child-in-forest? ff2) #t)

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          

; An Atom is one of: 
; – Number
; – String
; – Symbol 

; atom? : S-expr -> Boolean
; Is a given s-expression atomic data?

(define (atom? sexpr)
  (cond [(or (symbol? sexpr) (number? sexpr) (string? sexpr)) #t]
        [(list? sexpr) #f]))
        
(check-expect (atom? '5) #t)
(check-expect (atom? "hello") #t)
(check-expect (atom? 'nope) #t)
(check-expect (atom? '(this is (how we do it))) #f)

; count : S-expr Symbol -> Natural
; Determines how many times a given symbol appears in an s-expression

(define (count sexpr sym)
  (cond [(atom? sexpr) (if (symbol=? sym sexpr) 1 0)]
        [(list? sexpr) (foldr (λ (s r) (+ r (count s sym))) 0 sexpr)]))

(check-expect (count '() '()) 0)
(check-expect (count '(this is (how we do it)) 'barnacles) 0)
(check-expect (count 'hello 'hello) 1)
(check-expect (count '(i am (invincible invincible (invincible))) 'invincible) 3)
(check-expect (count '(howdy howdy howdy hey hi howdy) 'howdy) 4)

