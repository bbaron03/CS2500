;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Compute-Left-Margins) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; compute-left-margins : [List-of Strings] -> [List-of Numbers]
; produces a list of numbers indicating how many spaces are needed 
; on each line of text so that they are centered
(check-expect (compute-left-margins (list "Hello" "all students" "in" "CS2500")) (list 4 0 5 3))
(check-expect (compute-left-margins (list "My" "Name" "is" "Brendan")) (list 2 1 2 0))
(check-expect (compute-left-margins (list "Testing" "testing" "I" "Love" "To" "Test" "Functions"))
              (list 1 1 4 2 3 2 0))
(check-expect (compute-left-margins '()) '())
    
(define (compute-left-margins los)
  (local [; largest-of : [List-of String] -> String
          ; Determines the longest string in a list of strings
          (define (largest-of l)
            (cond [(empty? l) ""]
                  [(cons? l) (if (> (string-length (first l)) 
                                    (string-length (largest-of (rest l))))
                                 (first l)
                                 (largest-of (rest l)))]))
          
          (define largest-string (largest-of los))
          (define length-largest-s (string-length largest-string))
          
          ; middle-of: String -> Number
          ; Computes the index of the middle of a string
          (define (middle-of s)
            (ceiling (/ (string-length s) 2)))
          
          ; letters-left-of-mid : String -> Number
          ; Computes the number of letters to the left of the mniddle of a string
          (define (letters-left-of-mid s)
            (cond [(even? (string-length s)) (middle-of s)]
                  [(odd? (string-length s)) (sub1 (middle-of s))]))
          
          ; determine-left-margin String -> Number
          ; Computes the left margin of a single string
          (define (determine-left-margin s)
            (local [(define middle-largest-s (middle-of largest-string))]
                   (cond [(even? length-largest-s) (- middle-largest-s (letters-left-of-mid s))]
                         [(odd? length-largest-s) (- middle-largest-s (letters-left-of-mid s) 1)])))]
          (map determine-left-margin los)))                                                                     