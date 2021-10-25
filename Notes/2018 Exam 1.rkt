;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |2018 Exam 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An LOC (list of campaign contributions) is
; one of:
; - empty
; - (cons Contrib LOC)
(define-struct contrib [donor amount])
; A Contrib (campaign contribution) is one of:
; - Number
; - (make-contrib String Number)
; interpretation A simple number represents an
; anonymous contribution, while a
; (make-contrib d amt) represents a
; contribution of amt dollars from donor d.
(define contrib0 0)
(define contrib1 (make-contrib "Michael" 2800))
(define contrib2 (make-contrib "Omaha" 2700))
(define contrib3 48)
(define contrib4 68)
(define contrib5(make-contrib "Nebraska" 30593))

; contrib-temp: Contrib -> ?
(define (contrib-temp c)
  (cond [(number? c) ...]
        [(contrib? c) (... (contrib-donor c)
                           ... (contrib-amount c))]))

(define loc0 '())
(define loc1 (cons contrib0 (cons contrib2 loc0)))
(define loc2 (cons contrib3 loc1))
(define loc3 (cons contrib4 loc2))
(define loc4 (cons contrib5 loc1))

; loc-temp : [List-of Contrib] -> ?
(define (loc-temp loc)
  (... cond [(empty? loc) ...]
       [(cons? loc) ... (contrib-temp (first loc))
                    ... (loc-temp (rest loc)) ...]))

; any-bad-contrib? : [List-of Contrib] -> Boolean
; Checks, given a list of contributions, if any of the contributions on the list are illegal ones.
(check-expect (any-bad-contrib? loc0) #false)
(check-expect (any-bad-contrib? loc1) #false)
(check-expect (any-bad-contrib? loc2) #false)
(check-expect (any-bad-contrib? loc3) #true)
(check-expect (any-bad-contrib? loc4) #true)

(define (any-bad-contrib? loc)
  (cond [(empty? loc) false]
        [(cons? loc) (or (is-bad-contrib? (first loc))
                         (any-bad-contrib? (rest loc)))]))

; is-bad-contrib? : Contrib -> Boolean
; Is a contribution is illegal?
(check-expect (is-bad-contrib? contrib0) #false)
(check-expect (is-bad-contrib? contrib1) #true)
(check-expect (is-bad-contrib? contrib2) #false)
(check-expect (is-bad-contrib? contrib3) #false)
(check-expect (is-bad-contrib? contrib4) #true)
(check-expect (is-bad-contrib? contrib5) #true)

(define (is-bad-contrib? c)
  (cond [(number? c) (> c 50)]
        [(contrib? c) (> (contrib-amount c) 2700)]))

; any-bad-donors? : [List-of Contrib] -> Boolean
; Have any of the named donors on the list donated more than $2700?
(check-expect (any-bad-donors? loc0) #false)
(check-expect (any-bad-donors? loc1) #false)
(check-expect (any-bad-donors? (list contrib2 contrib2)) #true)
(check-expect (any-bad-donors? loc2) #false)
(check-expect (any-bad-donors? loc4) #true)

(define (any-bad-donors? loc)
  (cond [(empty? loc) #f]
        [(cons? loc) (or (bad-donor? (first loc) (rest loc))
                         (any-bad-donors? (rest loc)))]))

; bad-donor? : Contrib [List-of Contrib] -> Boolean
; Is the donor's total donation more than 2700?
(check-expect (bad-donor? contrib2 loc2) #true)
(check-expect (bad-donor? contrib5 '()) #true)
(check-expect (bad-donor? contrib2 (list contrib2 contrib2)) #true)
(check-expect (bad-donor? contrib3 (list contrib2 contrib3)) #false)
(check-expect (bad-donor? (make-contrib "NotaDonor" 15) loc4) #false)

(define (bad-donor? contrib loc)
  (local [; sum-donor-contribs : [List-of Contrib] String -> Number
          ; Computes the total amount of money donated by a given donor
          (define (sum-donor-contribs loc donor)
            (cond [(empty? loc) 0]
                  [(cons? loc) (+ (contrib-amount-if-donor donor (first loc))
                                      (sum-donor-contribs (rest loc) donor))]))

          ; contrib-amount-if-donor: String Contrib -> Number
          ; Produces amount if this donor has given name, else 0
          (define (contrib-amount-if-donor donor c)
            (cond [(number? c) 0]
                  [(contrib? c) (if (string=? (contrib-donor c) donor)
                                    (contrib-amount c) 0)]))]
    (cond [(number? contrib) #false]
          [(contrib? contrib) (> (+ (contrib-amount contrib)
                                    (sum-donor-contribs loc (contrib-donor contrib))) 2700)])))  


; map : {X Y} [X -> Y] [List-of X] -> [List-of Y]
; Creates a new list with the function f
; applied to each element
(check-expect (my-map add1 (list 1 2 3 4 5)) (list 2 3 4 5 6))
(check-expect (my-map string-length (list "marv" "ban")) (list 4 3))
(check-expect (my-map sqr '()) '())

(define (my-map f l)
  (local [; apply-f : X [List-of Y] -> [List-of Y]
          ; Apply the function to x and add it to the list
          (define (apply-f x l)
            (cons (f x) l))]
  (foldr apply-f '() l)))
