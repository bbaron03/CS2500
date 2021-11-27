;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab11Generative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define BG (empty-scene 500 500))
(define COLORS (list "red" "purple" "blue" "green" "yellow" "orange" "pink" "grey"))

; A KMCA (K-Means Clustering Assignment) is a
; (make-assignment [List-of Centroid] [List-of Nat] Boolean)
(define-struct assignment (centroids labels no-reassignment?))
; - where centroids is the current list of centroids (ordered from 0...k-1)
; - labels are the labels assigned to each datapoint
;   labels are indices into the centroids list
;   (the first datapoint is labeled with the first element in labels,
;    second point the second label, etc.)
; - and no-reassignment? keeps track of whether or not re-assignment has occurred
 
; A Centroid is a (make-centroid Number Number)
(define-struct centroid [x y])
; - where x is the x-coordinate of the centroid
; - and y is the y-coordinate of the centroid
 
; A Datapoint is a (make-datapoint Number Number)
(define-struct datapoint [x y])
; - where x is the x-coordinate of the data point
; - and y is the y-coordinate of the data point

; main : PosInt [List-of Datapoint] -> [List-of [List-of Datapoint]]
; run k-means clustering and output the datapoints binned into their respective clusters
(define (main k lop)
  (local
    [; next-assignment/main : KMCA -> KMCA
     ; Advance to the next assignment
     (define (next-assignment/main assignment)
       (next-assignment assignment k lop))
     ; draw/main : KMCA -> KMCA
     ; Draw the current clusters and points
     (define (draw/main assignment)
       (draw assignment k lop))]
    (cluster
     (assignment-labels
      (big-bang (initial-assignment k lop)
        [on-tick next-assignment/main 1]
        [to-draw draw/main]
        [stop-when assignment-no-reassignment?]))
     k lop)))

;; take : {X} Natural [List-of X] -> [List-of X]
;; Gets the first n elements of a list

(define (take n lox)
  (cond [(or (zero? n) (empty? lox)) '()]
        [(cons? lox) (cons (first lox) (take (sub1 n) (rest lox)))]))

(check-expect (take 3 '(1 2 3 4 5)) '(1 2 3))
(check-expect (take 0 '(hello world)) '())
(check-expect (take 7 '(#f #f #t #t)) '(#f #f #t #t))
(check-expect (take 3 '()) '())

;; distance : Centroid Datapoint -> Number
;; Computes the distance between a centroid and a datapoint

(define (distance centroid dp)
  (sqrt (+ (sqr (- (centroid-x centroid) (datapoint-x dp)))
           (sqr (- (centroid-y centroid) (datapoint-y dp))))))

(check-expect (distance (make-centroid 0 0) (make-datapoint 3 4)) 5)
(check-expect (distance (make-centroid 5 12) (make-datapoint 0 0)) 13)
(check-expect (distance (make-centroid 30 14) (make-datapoint 6 7)) 25)

;; compute-mean: [List-of Datapoint] -> Centroid
;; Computes the mean of the datapoints as a centroid

(define (compute-mean lod)
  (local [(define x-sum (foldr (λ (d n) (+ (datapoint-x d) n)) 0 lod))
          (define y-sum (foldr (λ (d n) (+ (datapoint-y d) n)) 0 lod))
          (define len (length lod))]
    (if (zero? len)
        (make-centroid 0 0)
        (make-centroid (/ x-sum len) (/ y-sum len))))) 
          
(check-expect (compute-mean '()) (make-centroid 0 0))
(check-expect (compute-mean (list (make-datapoint 0 0)
                                  (make-datapoint 5 2)
                                  (make-datapoint 2 3)
                                  (make-datapoint 8 9))) (make-centroid 15/4 7/2))
(check-expect (compute-mean (list (make-datapoint 0 0)
                                  (make-datapoint 5 2)
                                  (make-datapoint 2 3)
                                  (make-datapoint 8 9)
                                  (make-datapoint 5 16))) (make-centroid 4 6))

;; data-means: [List-of [List-of Datapoint]] -> [List-of Centroid]
;; Determine the centroid for each list of datapoints

(define (data-means lolod)
  (map compute-mean lolod))

(check-expect (data-means '()) '())
(check-expect (data-means (list '() (list (make-datapoint 0 0)
                                          (make-datapoint 5 2)
                                          (make-datapoint 2 3)
                                          (make-datapoint 8 9))))
              (list (make-centroid 0 0) (make-centroid 15/4 7/2)))

;; cluster: {X} [List-of Nat] Nat [List-of X] -> [List-of [List-of X]]
; Given a list of labels and the number of clusters, cluster lox
; (length labels) = (length lox)

(define (cluster labels k lox)
  (local [;; add-to-ans: X Nat [List-of [List-of X]] -> [List-of [List-of X]]
          ;; Add an element to the answer at the right index
          ;; Assume the index is less than the length of the answer
          (define (add-to-ans ele indx ans)
            (cond [(zero? indx) (cons (cons ele (first ans)) (rest ans))]
                  [(positive? indx) (cons (first ans) (add-to-ans ele (sub1 indx) (rest ans)))]))]
    (foldr add-to-ans (build-list k (λ (_) '())) lox labels)))

(check-expect (cluster (list 0 1 3 0 1 3)
                       4
                       (list "a" "b" "c" "d" "e" "f"))
              (list (list "a" "d")
                    (list "b" "e")
                    '()
                    (list "c" "f")))

; assign-new-labels : [List-of Centroid] Nat [List-of Datapoint]  -> [List-of Nat]
; Given the current centroids and the list of data points, output the new labels

(define (assign-new-labels centroids k lodp)
  (local [;; smallest-index [List-of Number] Number -> Natural
          ;; Gets the index of the smallest element, assume that the min val given is in the list
          (define (smallest-index lon min)
            (foldr (λ (val indx ans) (if (= val min) indx ans))
                   0
                   lon
                   (build-list (length lon) identity)))
          ;; label-index: Datapoint -> Nat
          ;; Find the index of the closest centroid
          (define (label-index dp)
            (local [(define dists (map (λ (c) (distance c dp)) centroids))]
              (smallest-index dists (apply min dists))))]
    (map label-index lodp)))

(check-expect (assign-new-labels (list (make-centroid -5 -5) (make-centroid 5 5))
                                 2
                                 (list (make-datapoint -20 -20)
                                       (make-datapoint -3 1)
                                       (make-datapoint 20 20)))
              (list 0 0 1))

;; initial-assignment: Natural [List-of Datapoint] -> KMCA
;; Determines the initial clustering for k clusters.

(define (initial-assignment k lod)
  (local [(define cents (map (λ (d) (make-centroid (datapoint-x d)
                                                   (datapoint-y d))) (take k lod)))]
    (make-assignment cents (assign-new-labels cents k lod) #f)))

(check-expect (initial-assignment 1 (list (make-datapoint -20 -20)
                                          (make-datapoint -3 1)
                                          (make-datapoint 20 20)))
              (make-assignment (list (make-centroid -20 -20)) (list 0 0 0) #f))
(check-expect (initial-assignment 3 (list (make-datapoint -20 -20)
                                          (make-datapoint -3 1)
                                          (make-datapoint 20 20)
                                          (make-datapoint 5 0)
                                          (make-datapoint -5 124)
                                          (make-datapoint 64 -2)))
              (make-assignment (list (make-centroid -20 -20)
                                     (make-centroid -3 1)
                                     (make-centroid 20 20))
                               (list 0 1 2 1 2 2) #f))

;; next-assignment : KMCA Natural [List-of Datapoint] -> KMCA
;; Determine the next assignments of the clusters

(define (next-assignment kmca k lod)
  (local [(define clustered (cluster (assignment-labels kmca) k lod))
          (define new-centroids (data-means clustered))
          (define new-labels (assign-new-labels new-centroids k lod))]
    (make-assignment new-centroids new-labels (equal? (assignment-centroids kmca)
                                                      new-centroids))))

(check-expect (next-assignment (make-assignment (list (make-centroid -20 -20)) (list 0 0 0) #f)
                               1
                               (list (make-datapoint -20 -20)
                                     (make-datapoint -3 1)
                                     (make-datapoint 20 20)))
              (make-assignment (list (make-centroid -1 1/3)) (list 0 0 0) #f))

(check-expect (next-assignment (make-assignment (list (make-centroid -1 1/3)) (list 0 0 0) #f)
                               1
                               (list (make-datapoint -20 -20)
                                     (make-datapoint -3 1)
                                     (make-datapoint 20 20)))
              (make-assignment (list (make-centroid -1 1/3)) (list 0 0 0) #t))

; draw-centroid : Centroid Color Image -> Image
; Draw a centroid on the given image
(check-expect
 (draw-centroid (make-centroid 0 0) "red" (circle 10 "solid" "black"))
 (place-image (text "X" 20 "red") 0 0 (circle 10 "solid" "black")))
(define (draw-centroid ctr color bg)
  (place-image (text "X" 20 color) (centroid-x ctr) (centroid-y ctr) bg))
 
; draw-point : Datapoint Color Image -> Image
; Draw a datapoint onto the given image
(check-expect
 (draw-point (make-datapoint 10 2) "red" (circle 10 "solid" "black"))
 (place-image (circle 3 "outline" "red") 10 2 (circle 10 "solid" "black")))
(define (draw-point dp color bg)
  (place-image (circle 3 "outline" color) (datapoint-x dp) (datapoint-y dp) bg))

; draw-shapes : [List-of X] [List-of Nat] [X Color Image -> Image] Image -> Image
; Draw shapes on bg at xs using colors from labels

(define (draw-shapes xs labels x->image bg)
  (foldr x->image bg xs (map (λ (i) (list-ref COLORS i)) labels)))

(check-expect (draw-shapes (list (make-centroid 0 0) (make-centroid 1 1))
                           (list 0 3)
                           draw-centroid
                           BG)
              (draw-centroid (make-centroid 0 0) (first COLORS)
                             (draw-centroid (make-centroid 1 1) (fourth COLORS)
                                            BG)))

;; draw-data: [List-of Datapoint] [List-of Nat] Image -> Image

(define (draw-data lod labels img)
  (draw-shapes lod labels draw-point img))

;; draw-centroids: [List-of Centroid] Nat Image -> Image

(define (draw-centroids cents k img)
  (draw-shapes cents (build-list k identity) draw-centroid img))

;; draw: KMCA Nat [List-of Datapoint] -> Image
(define (draw kmca k lod)
  (draw-centroids (assignment-centroids kmca) k (draw-data lod (assignment-labels kmca) BG)))

; random-datapoint : ? -> Datapoint
; Make a random datapoint
(define (random-datapoint _)
  (make-datapoint (random 500) (random 500)))
(define DATAPOINTS (build-list 200 random-datapoint))
(main 4 DATAPOINTS)
