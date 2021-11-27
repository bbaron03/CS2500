;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW11_Graphs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A {X} [Set-of X] is a [List-of X], where order does not matter and all elements are unique.
; A Graph is a (make-graph [Set-of Symbol] [Symbol -> [Set-of Symbol]])
(define-struct graph [nodes neighbors])
; and represents the nodes and edges in a graph.
; The neighbors function is only guaranteed to work for symbols that are in the
; set of nodes, and is guaranteed to return a set of symbols that are also in
; the set of nodes (i.e. the graph is well-formed).

;; Examples

(define graph1 (make-graph '(a b c d) (λ (s) (cond [(symbol=? s 'a) '(b)]
                                                   [(symbol=? s 'b) '(a)]
                                                   [(symbol=? s 'c) '(c d)]
                                                   [(symbol=? s 'd) '(a b)]))))
(define graph2 (make-graph '(a b c d) (λ (s) (cond [(symbol=? s 'a) '(b)]
                                                   [(symbol=? s 'b) '(d)]
                                                   [(symbol=? s 'c) '(d)]
                                                   [(symbol=? s 'd) '(a)]))))
(define graph3 (make-graph '(a b c d) (λ (s) (cond [(symbol=? s 'a) '(b c d)]
                                                   [(symbol=? s 'b) '(d)]
                                                   [(symbol=? s 'c) '(d)]
                                                   [(symbol=? s 'd) '(a)]))))
(define graph4 (make-graph '(a b c d e) (λ (s) (cond [(symbol=? s 'a) '(b)]
                                                     [(symbol=? s 'b) '(c)]
                                                     [(symbol=? s 'c) '(a d)]
                                                     [(symbol=? s 'd) '(e)]
                                                     [(symbol=? s 'e) '(a)]))))
(define graph5 (make-graph '(g h i j k) (λ (s) (cond [(symbol=? s 'g) '(h)]
                                                     [(symbol=? s 'h) '(i)]
                                                     [(symbol=? s 'i) '(g j)]
                                                     [(symbol=? s 'j) '(k)]
                                                     [(symbol=? s 'k) '(g)]))))

;; neighbor-of? : Graph Symbol Symbol -> Boolean
;; Is the second symbol a neighbor of the first?

(define (neighbor-of? graph s1 s2)
  (member? s2 ((graph-neighbors graph) s1)))

(check-expect (neighbor-of? graph1 'c 'c) #t)
(check-expect (neighbor-of? graph1 'a 'b) #t)
(check-expect (neighbor-of? graph2 'a 'c) #f)
(check-expect (neighbor-of? graph3 'a 'a) #f)

;; both-neighbors : Graph Symbol Symbol -> [Set-of Symbol]
;; Gets all neighbors of both symbols in the graph

(define (both-neighbors g s1 s2)
  (local [(define get-neighbors (graph-neighbors g))
          (define neighbors-s2 (get-neighbors s2))]
    (foldr (λ (n1 so-far) (if (member? n1 so-far)
                              so-far
                              (cons n1 so-far)))
           neighbors-s2 (get-neighbors s1))))

(check-expect (both-neighbors graph1 'a 'b) '(b a))
(check-expect (both-neighbors graph2 'b 'c) '(d))
(check-expect (both-neighbors graph3 'd 'a) '(a b c d))
(check-expect (both-neighbors graph4 'c 'e) '(d a))

;; graph=? : Graph Graph -> Boolean
;; Are two graphs equal to each other?

(define (graph=? g1 g2)
  (local [;; same-neighbors? : [Set-of Symbol] [Set-of Symbol] -> Boolean
          ;; Do two sets of symbols have the same elements
          (define (same-neighbors? s1 s2)
            (and (= (length s1) (length s2))
                 (andmap (λ (n1) (member? n1 s2)) s1)))]
    (and (= (length (graph-nodes g1)) (length (graph-nodes g2)))
         (andmap (λ (n1) (and (member? n1 (graph-nodes g2))
                              (same-neighbors? ((graph-neighbors g2) n1)
                                               ((graph-neighbors g1) n1)))) (graph-nodes g1)))))

(check-expect (graph=? graph1 graph1) #t)
(check-expect (graph=? graph2 graph3) #f)
(check-expect (graph=? graph2 graph4) #f)
(check-expect (graph=? graph2 graph2) #t)
(check-expect (graph=? graph1 graph3) #f)
(check-expect (graph=? graph4 graph5) #f)

; make-graph=? : Graph -> [Graph -> Boolean]
; Takes a graph and produces a function that checks if
; its argument is graph=? to the original graph
(define make-graph=? (λ (g1) (λ (g2) (graph=? g1 g2))))

;; collapse : Graph Symbol Symbol Symbol -> Graph
;; Assumes that the first two nodes are in the graph and the third is not.
;; Collapses the first two nodes into one new node named after the third symbol.

(define (collapse g s1 s2 new-s)
  (local [;; replace-symbols : [Set-of Symbol] -> [Set-of Symbol]
          ;; Replaces all instances of either the first or second symbol with the third
          (define (replace-symbols syms)
            (local [(define without (filter (λ (s) (not (or (symbol=? s1 s) (symbol=? s2 s)))) syms))]
              (if (or (member? s1 syms) (member? s2 syms))
                  (cons new-s without)
                  without)))]
    (make-graph (replace-symbols (graph-nodes g))
                (λ (s) (replace-symbols
                        (cond [(symbol=? s new-s) (both-neighbors g s1 s2)]
                              [else ((graph-neighbors g) s)]))))))

(check-satisfied (collapse graph4 'b 'c 'z)
                 (make-graph=? (make-graph '(z a d e) (λ (s) (cond [(symbol=? s 'a) '(z)]
                                                                   [(symbol=? s 'z) '(a d z)]
                                                                   [(symbol=? s 'd) '(e)]
                                                                   [(symbol=? s 'e) '(a)])))))
(check-satisfied (collapse graph2 'a 'c 'w)
                 (make-graph=? (make-graph '(w b d) (λ (s) (cond [(symbol=? s 'w) '(b d)]
                                                                 [(symbol=? s 'b) '(d)]
                                                                 [(symbol=? s 'd) '(w)])))))

;; reverse-edges : Graph -> Graph
;; Reverses the edges of the graph

(define (reverse-edges g)
  (make-graph (graph-nodes g)
              (λ (s) (filter (λ (n) (neighbor-of? g n s)) (graph-nodes g)))))

(check-satisfied (reverse-edges graph1)
                 (make-graph=? (make-graph '(a b c d) (λ (s) (cond [(symbol=? s 'a) '(b d)]
                                                                   [(symbol=? s 'b) '(a d)]
                                                                   [(symbol=? s 'c) '(c)]
                                                                   [(symbol=? s 'd) '(c)])))))
(check-satisfied (reverse-edges graph4)
                 (make-graph=? (make-graph '(a b c d e) (λ (s) (cond [(symbol=? s 'a) '(c e)]
                                                                     [(symbol=? s 'b) '(a)]
                                                                     [(symbol=? s 'c) '(b)]
                                                                     [(symbol=? s 'd) '(c)]
                                                                     [(symbol=? s 'e) '(d)])))))
;; rename : Graph [Set-of Symbol] -> Graph
;; Renames all the nodes in the graph to the new set, assuming they are
;; of the same length

(define (rename g new-names)
  (local [;; target-node : Symbol [Set-of Symbol] [Set-of Symbol] -> Symbol
          ;; Find the symbol of the corresponding symbol in the new-names
          (define (target-node target new old)
            (cond [(empty? new) (error "Unreachable")]
                  [(symbol=? target (first new)) (first old)]
                  [else (target-node target (rest new) (rest old))]))]
    (make-graph new-names
                (λ (s) (map (λ (n) (target-node n (graph-nodes g) new-names))
                            ((graph-neighbors g) (target-node s new-names (graph-nodes g))))))))

(check-satisfied (rename graph4 '(g h i j k))
                 (make-graph=? graph5))
(check-satisfied (rename graph1 '(z y x w))
                 (make-graph=? (make-graph '(z y x w) (λ (s) (cond [(symbol=? s 'z) '(y)]
                                                                   [(symbol=? s 'y) '(z)]
                                                                   [(symbol=? s 'x) '(x w)]
                                                                   [(symbol=? s 'w) '(z y)])))))

;; close? : Graph Symbol Symbol Natural -> Boolean
;; Is the second node within n steps of the first

(define (close? g from to steps)
  (local [;; close?/acc : Sybmol Natural [Set-of Symbol] -> Boolean
          ;; Is the symbol within n steps of the destination
          ;; Accumulator: Keeps track of all nodes visited so far to avoid
          ;; Must terminate because eventually there will be no more nodes that have not been visited
          (define (close?/acc from steps acc)
            (cond [(member? from acc) #f]
                  [(symbol=? from to) #t]
                  [(zero? steps) #f]
                  [else (ormap (λ (n) (close?/acc n (sub1 steps) (cons from acc)))
                               ((graph-neighbors g) from))]))]
    (close?/acc from steps '())))

(check-expect (close? graph1 'a 'a 0) #t)
(check-expect (close? graph1 'a 'a 1) #t)
(check-expect (close? graph1 'a 'b 0) #f)
(check-expect (close? graph1 'a 'b 1) #t)
(check-expect (close? graph4 'b 'd 1) #f)
(check-expect (close? graph4 'b 'd 2) #t)
(check-expect (close? graph4 'b 'd 4) #t)
(check-expect (close? graph4 'b 'a 2) #t)
(check-expect (close? graph4 'b 'e 2) #f)
(check-expect (close? graph4 'b 'e 3) #t)

;; find-all-paths : Graph Symbol Symbol -> [List-of [List-of Symbol]]
;; Find all paths between two symbols

(define (find-all-paths g from to)
  (local [;; find-all-paths/acc : Symbol [List-of Symbol] -> [List-of [List-of Symbol]]
          ;; Find all paths to the destination from a symbol avoiding visited nodes
          (define (find-all-paths/acc from avoid)
            (cond [(member? from avoid) '()]
                  [(symbol=? from to) (list (reverse (cons to avoid)))]
                  [else (apply append (map (λ (n) (find-all-paths/acc n (cons from avoid)))
                                           ((graph-neighbors g) from)))]))]
    (find-all-paths/acc from '())))

(check-expect (find-all-paths graph4 'a 'a) '((a)))
(check-expect (find-all-paths graph4 'b 'a) '((b c a) (b c d e a)))
(check-expect (find-all-paths graph1 'a 'b) '((a b)))
(check-expect (find-all-paths graph1 'a 'c) '())
(check-expect (find-all-paths graph1 'c 'b) '((c d a b) (c d b)))

;; connected? : Graph -> Boolean
;; Is every node connected to every other node

(define (connected? g)
  (local [(define nodes (graph-nodes g))
          (define steps (length nodes))]
    (andmap (λ (n1) (andmap (λ (n2) (close? g n1 n2 steps)) nodes)) nodes)))

(check-satisfied graph1 (compose not connected?))
(check-satisfied graph2 (compose not connected?))
(check-satisfied graph3 connected?)
(check-satisfied graph4 connected?)
(check-satisfied graph5 connected?)
(check-satisfied (make-graph '(a b) (λ (_) '())) (compose not connected?))

;; undirected? : Graph
;; Does every edge have a matching edge in the other direction?

(define (undirected? g)
  (andmap (λ (n1) (andmap (λ (n2) (neighbor-of? g n2 n1))
                          ((graph-neighbors g) n1)))
          (graph-nodes g)))

(check-satisfied graph1 (compose not undirected?))
(check-satisfied graph2 (compose not undirected?))
(check-satisfied graph3 (compose not undirected?))
(check-satisfied graph4 (compose not undirected?))
(check-satisfied graph5 (compose not undirected?))
(check-satisfied (make-graph '(a b c) (λ (s) (cond [(symbol=? s 'a) '(b c)]
                                                   [(symbol=? s 'b) '(a c)]
                                                   [(symbol=? s 'c) '(a b)])))
                 undirected?)
(check-satisfied (make-graph '(a b c) (λ (s) (cond [(symbol=? s 'a) '(a b c)]
                                                   [(symbol=? s 'b) '(a)]
                                                   [(symbol=? s 'c) '(a)])))
                 undirected?)

;; graph-shape=? : Graph Graph -> Boolean
;; Are two graphs the same shape?

(define (graph-shape=? g1 g2)
  (local [;; similar-nodes?/acc : Symbol Symbol [Set-of Symbol]
          ;; Check if two nodes from the graphs are similar
          (define (similar-nodes?/acc n1 n2 avoid)
            (cond [(member? n1 avoid) #t]
                  [else (local [(define neighbors1 ((graph-neighbors g1) n1))
                                (define neighbors2 ((graph-neighbors g2) n2))]
                          (and (= (length neighbors1) (length neighbors2))
                               (boolean=? (member? n1 neighbors1) (member? n2 neighbors2))
                               (andmap (λ (next1)
                                         (ormap (λ (next2)
                                                  (similar-nodes?/acc next1 next2 (cons n1 avoid)))
                                                neighbors2))
                                       neighbors1)))]))]
    (and (= (length (graph-nodes g1)) (length (graph-nodes g2)))
         (andmap (λ (n1) (ormap (λ (n2) (similar-nodes?/acc n1 n2 '()))
                                (graph-nodes g2)))
                 (graph-nodes g1)))))

(check-expect (graph-shape=? graph1 graph1) #t)
(check-expect (graph-shape=? graph2 graph2) #t)
(check-expect (graph-shape=? graph4 graph5) #t)
(check-expect (graph-shape=? graph1 graph2) #f)
(check-expect (graph-shape=? (make-graph '(a b c) (λ (s) (cond [(symbol=? s 'a) '(b)]
                                                               [(symbol=? s 'b) '(a)]
                                                               [(symbol=? s 'c) '(a b)])))
                             (make-graph '(a b c) (λ (s) (cond [(symbol=? s 'a) '(a)]
                                                               [(symbol=? s 'b) '(b)]
                                                               [(symbol=? s 'c) '(a b)]))))
              #f)
(check-expect (graph-shape=? (make-graph '(a b c) (λ (s) (cond [(symbol=? s 'a) '(b)]
                                                               [(symbol=? s 'b) '(a)]
                                                               [(symbol=? s 'c) '(a b)])))
                             (make-graph '(a b c) (λ (s) (cond [(symbol=? s 'a) '(a)]
                                                               [(symbol=? s 'b) '(b)]
                                                               [(symbol=? s 'c) '(a b)]))))
              #f)
(check-expect (graph-shape=? (make-graph '(a b c d) (λ (s) (cond [(symbol=? s 'a) '(b)]
                                                                 [(symbol=? s 'b) '(d)]
                                                                 [(symbol=? s 'c) '(d)]
                                                                 [(symbol=? s 'd) '(a)])))
                             (make-graph '(d e f g) (λ (s) (cond [(symbol=? s 'd) '(f)]
                                                                 [(symbol=? s 'e) '(d)]
                                                                 [(symbol=? s 'f) '(d)]
                                                                 [(symbol=? s 'g) '(d)]))))
              #f)
(check-expect (graph-shape=? (make-graph '(a b c d) (λ (s) (cond [(symbol=? s 'a) '(b)]
                                                                 [(symbol=? s 'b) '(d)]
                                                                 [(symbol=? s 'c) '(d)]
                                                                 [(symbol=? s 'd) '(a)])))
                             (make-graph '(d e g f) (λ (s) (cond [(symbol=? s 'd) '(e)]
                                                                 [(symbol=? s 'e) '(g)]
                                                                 [(symbol=? s 'f) '(g)]
                                                                 [(symbol=? s 'g) '(d)]))))
              #t)

