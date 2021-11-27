;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Hw9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct pair [fst snd])
; a [Pair-of A B] is a (make-pair A B)
 
; A Type is one of:
; - 'number
; - 'boolean
; - 'string

(define-struct list-ty [fst])
; - make-list-ty Type
(define-struct pair-ty [fst snd])
; - (make-pair-ty Type Type)
(define-struct fun-ty [arg ret])
; - (make-fun-ty Type Type)
 
; Interpretation: a Type represents different types of data we use in our programs.
; In particular, these are some of the types we write in our signatures.
 
(define (type-temp type)
  (cond [(equal? type 'number) ...]
        [(equal? type 'boolean) ...]
        [(equal? type 'string) ...]
        [(list-ty? type) (... (type-temp (list-ty-fst type)) ...)]
        [(pair-ty? type) (... (type-temp (pair-ty-fst type)) ...
                              (type-temp (pair-ty-snd type)) ...)]
        [(fun-ty? type) (... (type-temp (fun-ty-arg type)) ...
                             (type-temp (fun-ty-ret type)) ...)]))
 
(define Number 'number)
(define Boolean 'boolean)
(define String 'string)
(define (Pair-of A B) (make-pair-ty A B))
(define (Function X Y) (make-fun-ty X Y))
(define (List-of X) (make-list-ty X))
 
 
; enforce : Type X -> X
; ensures the argument x behaves like the type,
; erroring otherwise (either immediately or when used)
(define (enforce type x)
  (local ((define (err _) (error "the type didn't match: "
                                 x " : " (type->string type))))
    (cond [(equal? type 'number) (if (number? x) x (err 1))]
          [(equal? type 'boolean) (if (boolean? x) x (err 1))]
          [(equal? type 'string) (if (string? x) x (err 1))]
          [(list-ty? type) (if (list? x)
                               (cond [(empty? x)'()]
                                     [(cons? x) (cons (enforce (list-ty-fst type) (first x))
                                                      (enforce type (rest x)))])
                               (err 1))]
          [(pair-ty? type) (if (pair? x)
                               (make-pair
                                (enforce (pair-ty-fst type) (pair-fst x))
                                (enforce (pair-ty-snd type) (pair-snd x)))
                               (err 1))]
          [(fun-ty? type)
           (if (procedure? x)
               (lambda (y)
                 (local ((define arg (enforce (fun-ty-arg type) y)))
                   (enforce (fun-ty-ret type) (x arg))))
               (err 1))])))


(check-expect (enforce (List-of Number) (list 5 5)) (list 5 5))
(check-expect (enforce (List-of String) (list "hello" "I" "am robot" "man"))
              (list "hello" "I" "am robot" "man"))
(check-error (enforce (List-of Boolean) (list "not a boolean list")))
(check-expect (enforce (List-of (Pair-of Number Number)) (list (make-pair 1 2) (make-pair 3 4)))
              (list (make-pair 1 2) (make-pair 3 4)))
(check-error (map (λ (f) (f 1)) (enforce (List-of (Function Number String))
                                         (list (λ (x) (number->string x))
                                               (λ (y) y)))))
(check-expect (map (λ (f) (f 1)) (enforce (List-of (Function Number String))
                                          (list (λ (x) (number->string x))
                                                (λ (y) (number->string y))))) (list "1" "1"))
(check-expect (map (λ (f) (f 10)) (enforce (List-of (Function Number Number))
                                           (list (λ (x) x)
                                                 (λ (y) y)))) (list 10 10))
(check-expect (enforce Number 1) 1)
(check-error (enforce Number "hi"))
(check-expect (enforce Boolean #true) #true)
(check-expect (enforce String "hi") "hi")
(check-error (enforce String 34))
(check-expect (enforce (Pair-of Number Number) (make-pair 1 2)) (make-pair 1 2))
(check-error (enforce (Pair-of Number String) 1))
(check-expect ((enforce (Function Number Number) (lambda (x) x)) 1) 1)
(check-error ((enforce (Function Number Number) (lambda (x) x)) "hi"))
(check-error ((enforce (Function Number String) (lambda (x) x)) 1))

; sum-list : [List-of Number] -> Number
; Adds up the numbers in the list
(define sum-list (enforce (Function (List-of Number) Number)
                          (λ (l) (cond
                                   [(empty? l) 0]
                                   [(cons? l) (+ (first l) (sum-list (first l)))]))))

(check-error (sum-list (list 5 10 15 20)))
(check-error (sum-list (list #t #f)))
(check-expect (sum-list '()) 0)
 
; contains-frog? : [List-of String] -> Boolean
; Returns whether or not the list contains "frog"
(define contains-frog? (enforce (Function (List-of String) Boolean)
                                (λ (l)
                                  (cond
                                    [(empty? l) "false"]
                                    [(cons? l) (or (string=? (first l) "frog")
                                                   (contains-frog? (first l)))]))))

(check-error (contains-frog? (list "Yes" "I" "dont" "work")))
(check-error (contains-frog? (list 5 10 15 20)))
(check-error (contains-frog? '()))

; type->string : Type -> String
; Changes a type into an appropriate string
(define (type->string type)
  (cond [(equal? type 'number) "Number"]
        [(equal? type 'boolean) "Boolean"]
        [(equal? type 'string) "String"]
        [(list-ty? type) (string-append "[List-of " (type->string (list-ty-fst type)) "]")]
        [(pair-ty? type) (string-append "[Pair-of " (type->string (pair-ty-fst type))
                                        " " (type->string (pair-ty-snd type)) "]")]
        [(fun-ty? type) (string-append "[" (type->string (fun-ty-arg type)) " -> "
                                       (type->string (fun-ty-ret type)) "]")]))


(check-expect (type->string String)
              "String")
(check-expect (type->string (List-of (Function Number Number)))
              "[List-of [Number -> Number]]")
(check-expect (type->string (Pair-of Number Boolean))
              "[Pair-of Number Boolean]")
(check-expect (type->string (Function (Function Number Number) String))
              "[[Number -> Number] -> String]")
