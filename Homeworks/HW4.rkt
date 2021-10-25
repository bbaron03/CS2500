;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List of Integers(LOI) is one of:
; - '()
; - (cons Integer LOI)
; Interpretation: A list of integers that could be empty or a cons

; Examples
(define loi1 '())
(define loi2 (cons 5 loi1))
(define loi3 (cons 8 loi2))
(define loi4 (cons -10 loi3))
(define loi5 (cons 5 loi4))

; Template
; loi-temp: LOI -> ?
#;(define (loi-temp loi)
    (cond [(empty? loi) ...]
          [(cons? loi) ... (first loi) ...
                       ... (loi-temp (rest loi) ...)]))

; not-present?: LOI Integer -> Boolean
; Determines whether an integer is not in a LOI
(check-expect (not-present? loi1 5) #true)
(check-expect (not-present? loi2 -10) #true)
(check-expect (not-present? loi3 5) #false)
(check-expect (not-present? loi4 -10) #false)
(check-expect (not-present? loi5 8) #false)

(define (not-present? loi number)
  (cond [(empty? loi) #true]
        [(cons? loi) (and (not (= number (first loi)))
                          (not-present? (rest loi) number))]))

; all-unique?: LOI -> Boolean
; Determines if an LOI contains all unique values
(check-expect (all-unique? loi1) #true)
(check-expect (all-unique? loi3) #true)
(check-expect (all-unique? loi5) #false)

(define (all-unique? loi)
  (cond [(empty? loi) #true]
        [(cons? loi) (and (not-present? (rest loi) (first loi))
                          (all-unique? (rest loi)))]))

; has-even-count?: LOI Integer -> Boolean
; Determines whether an integer is in a LOI an even amount of times
(check-expect (has-even-count? loi3 8) #false)
(check-expect (has-even-count? loi1 5) #true)
(check-expect (has-even-count? loi5 5) #true)

(define (has-even-count? loi int)
  (cond [(empty? loi) #true]
        [(cons? loi) (if (= (first loi) int)
                         (not (has-even-count? (rest loi) int))
                         (has-even-count? (rest loi) int))]))

;;Exercise 5
#|
Because of the way recursion works in racket, a theoretical all--even-count? function cannot
be made in the same way as all-unique?. That all-even-count? would go through the LoI and for
each integer in the LoI, it would check if there was an even count of that int in the remainder
of the list. However, that function would not be able to check the integers that came before it
in the list. It can only move down the stack while checking, and return its answers up the
stack. This is a problem because there could be an unknown number of the integer being checked
higher up in the list where the function is not checking, making it impossible to determine
the parity of the count of every integer in the list. If one wanted to make an all-even-count?
function, they would have to create a duplicate LoI, and iterate over that list instead, and
perform has-even-count? on the original LoI for all integers in the duplicate.
|#

; Love Thy Neighbor

; An Intersection is a:
; - (make-posn Integer Integer) from 0 to 99 inclusive
; Interpretation: An Intersection is where two roads meet in a grid-like city map where the
; origin is in the bottom left

; Examples
(define intersection1 (make-posn 40 68))
(define intersection2 (make-posn 0 0))
(define intersection3 (make-posn 38 99))
(define intersection4 (make-posn 56 43))

; Temp
; intersection-temp: Intersection -> ?
(define (intersection-temp intersection)
  (... (posn-x intersection) ... (posn-y intersection) ...))

; A LoInter is either one of
; - '()
; - (cons Intersection LoInter)
; A list of Intersections or an empty list

; Examples
(define loInter1 '())
(define loInter2 (cons intersection1 loInter1))
(define loInter3 (cons intersection2 loInter2))
(define loInter4 (cons intersection3 loInter3))
(define loInter5 (cons intersection4 loInter4))

; loInter-temp: LoInter -> ?
(define (loInter-temp loInter)
  (... cond [(empty? loInter) ...]
       [(cons? loInter) ... (intersection-temp (first loInter)) ...
                        ... (loInter-temp (rest loInter)) ...]))

; intersection-in-range?: Intersection -> Boolean
; Determines if an intersection is on the grid x= [0,99], y=[0,99]
(check-expect (intersection-in-range? intersection1) #true)
(check-expect (intersection-in-range? intersection2) #true)
(check-expect (intersection-in-range? intersection3) #true)
(check-expect (intersection-in-range? intersection4) #true)
(check-expect (intersection-in-range? (make-posn -1 10)) #false)
(check-expect (intersection-in-range? (make-posn 50 -15)) #false)
(check-expect (intersection-in-range? (make-posn 100 100)) #false)

(define (intersection-in-range? intersection)
  (and (<= 0 (posn-x intersection) 99) (<= 0 (posn-y intersection) 99)))

; neighbor-north: Intersection -> Intersection
; Determines the northern neighbor, if any, of an Intersection 
(check-expect (neighbor-north intersection1) (make-posn 40 69))
(check-expect (neighbor-north intersection2) (make-posn 0 1))
(check-expect (neighbor-north intersection3) (make-posn 38 100))
(check-expect (neighbor-north intersection4) (make-posn 56 44))

(define (neighbor-north intersection)
  (make-posn (posn-x intersection) (add1 (posn-y intersection))))
     
; neighbor-east: Intersection -> Intersection
; Determines the eastern neighbor, if any, of an Intersection
(check-expect (neighbor-east intersection1) (make-posn 41 68))
(check-expect (neighbor-east intersection2) (make-posn 1 0))
(check-expect (neighbor-east intersection3) (make-posn 39 99))
(check-expect (neighbor-east intersection4) (make-posn 57 43))

(define (neighbor-east intersection)
  (make-posn (add1 (posn-x intersection)) (posn-y intersection)))

; neighbor-west: Intersection -> Intersection
; Determines the western neighbor, if any, of an Intersection
(check-expect (neighbor-west intersection1) (make-posn 39 68))
(check-expect (neighbor-west intersection2) (make-posn -1 0))
(check-expect (neighbor-west intersection3) (make-posn 37 99))
(check-expect (neighbor-west intersection4) (make-posn 55 43))

(define (neighbor-west intersection)
  (make-posn (sub1 (posn-x intersection)) (posn-y intersection)))

; neighbor-south: Intersection -> Intersection
; Determines the southern neighbor, if any, of an Intersection
(check-expect (neighbor-south intersection1) (make-posn 40 67))
(check-expect (neighbor-south intersection2) (make-posn 0 -1))
(check-expect (neighbor-south intersection3) (make-posn 38 98))
(check-expect (neighbor-south intersection4) (make-posn 56 42))

(define (neighbor-south intersection)
  (make-posn (posn-x intersection) (sub1 (posn-y intersection))))

; neighbors-all: Intersection -> LoInter
; Determines the 4 possible neighbors, if they exist, of a given intersection
(define neighbors1 (cons (neighbor-north intersection1)
                         (cons (neighbor-south intersection1)
                               (cons (neighbor-east intersection1)
                                     (cons (neighbor-west intersection1) '())))))
(define neighbors2 (cons (neighbor-north intersection2)
                         (cons (neighbor-south intersection2)
                               (cons (neighbor-east intersection2)
                                     (cons (neighbor-west intersection2) '())))))
(define neighbors3 (cons (neighbor-north intersection3)
                         (cons (neighbor-south intersection3)
                               (cons (neighbor-east intersection3)
                                     (cons (neighbor-west intersection3) '())))))
(define neighbors4 (cons (neighbor-north intersection4)
                         (cons (neighbor-south intersection4)
                               (cons (neighbor-east intersection4)
                                     (cons (neighbor-west intersection4) '())))))

(check-expect (neighbors-all intersection1) neighbors1)
(check-expect (neighbors-all intersection2) neighbors2)
(check-expect (neighbors-all intersection3) neighbors3)
(check-expect (neighbors-all intersection4) neighbors4)
             
(define (neighbors-all intersection)
  (cons (neighbor-north intersection)
        (cons (neighbor-south intersection)
              (cons (neighbor-east intersection)
                    (cons (neighbor-west intersection) '())))))

; valid-neighbors: LoInter -> LoInter
; Removes invalid intersections(outside the range) from a LoInter
(define v-neighbors1 (cons (neighbor-north intersection1)
                           (cons (neighbor-south intersection1)
                                 (cons (neighbor-east intersection1)
                                       (cons (neighbor-west intersection1) '())))))
(define v-neighbors2 (cons (neighbor-north intersection2)
                           (cons (neighbor-east intersection2) '())))
(define v-neighbors3 (cons (neighbor-south intersection3)
                           (cons (neighbor-east intersection3)
                                 (cons (neighbor-west intersection3) '()))))
(define v-neighbors4 (cons (neighbor-north intersection4)
                           (cons (neighbor-south intersection4)
                                 (cons (neighbor-east intersection4)
                                       (cons (neighbor-west intersection4) '())))))
(check-expect (valid-neighbors (neighbors-all intersection1)) v-neighbors1)
(check-expect (valid-neighbors (neighbors-all intersection2)) v-neighbors2)
(check-expect (valid-neighbors (neighbors-all intersection3)) v-neighbors3)
(check-expect (valid-neighbors (neighbors-all intersection4)) v-neighbors4)

(define (valid-neighbors loInter)
  (cond [(empty? loInter) '()]
        [(cons? loInter) (if (intersection-in-range? (first loInter))
                             (cons (first loInter) (valid-neighbors (rest loInter)))
                             (valid-neighbors (rest loInter)))]))

; Double bag or single bag

; A Cost is one of:
; - (make-unit Number)
; - (make-lb Number)
(define-struct unit [cost])
(define-struct lb [cost])
; and represents either the cost per unit or per lb of an item

; Examples
(define cost1 (make-unit 15))
(define cost2 (make-unit 11.50))
(define cost3 (make-lb .99))
(define cost4 (make-lb 8.75))

; Template
; cost-temp: Cost -> ?
(define (cost-temp cost)
  (... cond [(unit? cost) ... (unit-cost cost) ...]
       [(lb? cost) ... (lb-cost cost) ...]))

; A CE (CatalogueEntry) is a
; (make-ce String Cost)
(define-struct ce [name cost])
; and represents the name of a food and how much it costs

; Examples
(define ce1 (make-ce "Rice" cost1))
(define ce2 (make-ce "Tomatoes" cost2))
(define ce3 (make-ce "Pasta" cost3))
(define ce4 (make-ce "Onions" cost4))

; Template
; ce-temp: CE -> ?
(define (ce-temp ce)
  (... (ce-name ce) ... (cost-temp (ce-cost ce)) ...))

; A GC (GroceryCatalogue) is one of:
; - '()
; - (cons CE GC)
; where each catalogue entry has a unique name

; Examples
(define gc1 '())
(define gc2 (cons ce1 gc1))
(define gc3 (cons ce2 gc2))
(define gc4 (cons ce3 gc3))
(define gc5 (cons ce4 gc4))

; Template
; gc-temp: GC -> ?
(define (gc-temp gc)
  (... cond [(empty? gc) ...]
       [(cons? gc) ... (ce-temp (first gc)) ...
                   ... (gc-temp (rest gc)) ...]))

; A Order is one of:
; - String
; - (make-weight String Number)
(define-struct weight [name lb])
; and represents either one unit of food or its name and weight in lbs

; Examples
(define order1 "Tomatoes")
(define order2 (make-weight "Pasta" 5))
(define order3 (make-weight "Onions" 10.5))

; Template
; order-temp: Order -> ?
(define (order-temp order)
  (... cond [(string? order) ...]
       [(weight? order) ... (weight-name order) ...
                        ... (weight-lb order) ...]))

; A Checkout is one of:
; - '()
; - (cons Order Checkout)

; Examples
(define checkout1 '())
(define checkout2 (cons order1 checkout1))
(define checkout3 (cons order2 checkout2))
(define checkout4 (cons order3 checkout3))
(define checkout5 (make-list 11 order2))

; Template
; checkout-temp: Checkout -> ?
(define (checkout-temp checkout)
  (... cond [(empty? checkout) ...]
       [(cons? checkout) ... (order-temp (first checkout)) ...
                         ... (checkout-temp (rest checkout)) ...]))

; get-cost : String GC -> String
; Gets the cost of an item in a GC, if the item is not in the gc,
; it will cause an error
(define ERROR-MSG-NOT-FOUND "Not Such Item in the Grocery Catalogue")

(check-error (get-cost "Rice" gc1) ERROR-MSG-NOT-FOUND)
(check-expect (get-cost "Rice" gc2) cost1)
(check-expect (get-cost "Tomatoes" gc3) cost2)
(check-expect (get-cost "Pasta" gc4) cost3)
(check-expect (get-cost "Onions" gc5) cost4)

(define (get-cost str gc)
  (cond [(empty? gc) (error ERROR-MSG-NOT-FOUND)]
        [(cons? gc) (if (string=? (ce-name (first gc)) str)
                        (ce-cost (first gc))
                        (get-cost str (rest gc)))]))

; set-cost: String Cost GC -> GC
; Changes the cost of a given item to the cost provided in a GC
(check-expect (set-cost "Rice" cost2 gc2) (cons (make-ce "Rice" cost2) gc1))
(check-expect (set-cost "Tomatoes" cost1 gc3) (cons (make-ce "Tomatoes" cost1) gc2))
(check-expect (set-cost "Tomatoes" cost4 gc1) '())
(check-expect (set-cost "NotAFruit" cost3 gc5) gc5)

(define (set-cost name cost gc)
  (cond [(empty? gc) '()]
        [(cons? gc) (if (string=? (ce-name (first gc)) name)
                        (cons (make-ce name cost) (set-cost name cost (rest gc)))
                        (cons (first gc) (set-cost name cost (rest gc))))]))

; average-unit-cost: GC -> Number
; Determines the average cost of items priced per unit
(check-error (average-unit-cost gc1) ERROR-MSG-NOT-FOUND)
(check-expect (average-unit-cost gc2) 15)
(check-expect (average-unit-cost gc3) (/ (+ 15 11.50) 2))
(check-expect (average-unit-cost gc4) (/ (+ 15 11.50) 2))

(define (average-unit-cost gc)
  (if (not (= (total-unit-costs gc) 0))
      (/ (sum-unit-costs gc) (total-unit-costs gc))
      (error ERROR-MSG-NOT-FOUND)))

; total-unit-costs : GC -> NaturalNumber
; Determines how many, if any, items are priced per unit in a GC
(check-expect (total-unit-costs gc1) 0)
(check-expect (total-unit-costs gc2) 1)
(check-expect (total-unit-costs gc3) 2)
(check-expect (total-unit-costs gc4) 2)

(define (total-unit-costs gc)
  (cond [(empty? gc) 0]
        [(cons? gc) (if (unit? (ce-cost (first gc)))
                        (+ 1 (total-unit-costs (rest gc)))
                        (total-unit-costs (rest gc)))]))
                        
; sum-unit-costs : GC -> Number
; Computes the sum of the unit costs in a GC
(check-expect (sum-unit-costs gc1) 0)
(check-expect (sum-unit-costs gc2) (unit-cost cost1))
(check-expect (sum-unit-costs gc3) (+ (unit-cost cost2)
                                      (unit-cost cost1)))
(check-expect (sum-unit-costs gc4) (+ (unit-cost cost2)
                                      (unit-cost cost1)))
(define (sum-unit-costs gc)
  (cond [(empty? gc) 0]
        [(cons? gc) (+ (get-unit-cost (ce-cost (first gc))) (sum-unit-costs (rest gc)))]))

; get-unit-cost: Cost -> Number
; Determines the per unit cost of a Cost, if any exists.
(check-expect (get-unit-cost (ce-cost ce1)) 15)
(check-expect (get-unit-cost (ce-cost ce2)) 11.5)
(check-expect (get-unit-cost (ce-cost ce3)) 0)

(define (get-unit-cost cost)
  (if (unit? cost)
      (unit-cost cost)
      0))

; express-lane? : Checkout -> Boolean
; Determines if a checkout has ten items or fewer, and therefore if they
; are elligible for the express lane checkout
(check-expect (express-lane? checkout1) #true)
(check-expect (express-lane? checkout2) #true)
(check-expect (express-lane? checkout3) #true)
(check-expect (express-lane? checkout4) #true)
(check-expect (express-lane? checkout5) #false)

(define (express-lane? checkout)
  (<= (sum-orders checkout) 10))
  
; sum-orders: Checkout -> NaturalNumber
; Computes the amount of orders in a checkout
(check-expect (sum-orders checkout1) 0)
(check-expect (sum-orders checkout2) 1)
(check-expect (sum-orders checkout3) 2)
(check-expect (sum-orders checkout4) 3)
(check-expect (sum-orders checkout5) 11)

(define (sum-orders checkout)
  (cond [(empty? checkout) 0]
        [(cons? checkout) (add1 (sum-orders (rest checkout)))]))

; total-cost: Checkout GC -> Number
; Computes the total cost of a checkout
(check-expect (total-cost checkout1 gc1) 0)
(check-expect (total-cost checkout2 gc3) 11.50)
(check-expect (total-cost checkout5 gc5) 54.45)
(check-expect (total-cost checkout4 gc5) 108.325)
(check-error (total-cost (cons "Macaroni" '()) gc5) ERROR-MSG-NOT-FOUND)
(check-error (total-cost (cons "Onions" '()) gc5) NO-WEIGHT-ERROR-MSG)
(check-error (total-cost (cons (make-weight "Tomatoes" 10) '()) gc5) WEIGHT-ASSIGNED-ERROR-MSG)

(define (total-cost checkout gc)
  (cond [(empty? checkout) 0]
        [(cons? checkout) (+ (determine-item-cost (get-cost (get-order-name (first checkout)) gc)
                                                  (first checkout))
                             (total-cost (rest checkout) gc))]))

; get-order-name : Order -> String
; Determines the name of an item in an order
(check-expect (get-order-name order1) order1)
(check-expect (get-order-name order2) (weight-name order2))
(check-expect (get-order-name order3) (weight-name order3))

(define (get-order-name order)
  (cond [(string? order) order]
        [(weight? order) (weight-name order)]))
  
; determine-item-cost Cost Order -> Number
; Determines the cost of an item
(check-expect (determine-item-cost cost1 order1) (unit-cost cost1))
(check-expect (determine-item-cost cost3 order2) (* (lb-cost cost3) (weight-lb order2)))
(check-error (determine-item-cost cost1 order2) WEIGHT-ASSIGNED-ERROR-MSG)
(check-error (determine-item-cost cost3 order1) NO-WEIGHT-ERROR-MSG)

(define (determine-item-cost cost order)
  (cond [(string? order) (string-order-cost cost)]
        [(weight? order) (weight-lb-cost cost order)]))

; string-unit-cost: Cost -> Number
; Computes the cost of a per unit priced item, and throws an error if the item
; is priced per lb
(define NO-WEIGHT-ERROR-MSG "No Weight Assigned to Per lb Priced Item")
(check-expect (string-order-cost cost1) (unit-cost cost1))
(check-expect (string-order-cost cost2) (unit-cost cost2))
(check-error (string-order-cost cost3) NO-WEIGHT-ERROR-MSG)

(define (string-order-cost cost)
  (if (= (get-unit-cost cost) 0)
      (error NO-WEIGHT-ERROR-MSG)
      (get-unit-cost cost)))

; weight-lb-cost: Cost Order -> Number
; Computes the cost of a per lb unit priced times its weight in pounds,
; throws an error if the item is a per unit priced item.
(define WEIGHT-ASSIGNED-ERROR-MSG "Weight assigned to Per Unit Priced Item")
(check-expect (weight-lb-cost cost3 order3) (* (lb-cost cost3) (weight-lb order3)))
(check-expect (weight-lb-cost cost4 order2) (* (lb-cost cost4) (weight-lb order2)))
(check-error (weight-lb-cost cost2 order2) WEIGHT-ASSIGNED-ERROR-MSG)

(define (weight-lb-cost cost order)
  (if (not (lb? cost))
      (error WEIGHT-ASSIGNED-ERROR-MSG)
      (* (lb-cost cost) (weight-lb order))))
