#lang racket

(provide (all-defined-out)) ; export all top-level definitions for testing

(require racket/trace)


;; 1. Integer exponentiation
(define (iexpt n e)
  (cond
    ((= e 0) 1)
    ((= e 1) n)
    (else (* n (iexpt n (- e 1))))))


;; 2. Polynomial evaluation
(define (poly-eval coeffs x)
  (define (poly-helper coeffs power acc)
    (cond
      ((null? coeffs) acc)
      (else (poly-helper (cdr coeffs) (+ power 1) (+ acc (* (car coeffs) (iexpt x power)))))))
  (poly-helper coeffs 0 0))


;; 3. List concatenation
(define (concatenate . lsts)
  (let rec ([restlists lsts]
            [acc '()])
    (if (empty? restlists)
        (reverse acc)
        (rec (rest restlists) (concatenate-helper acc (first restlists))))))

(define (concatenate-helper acc list)
    (if (empty? list)
      acc
      (concatenate-helper (cons (first list) acc) (rest list))))


;; 4. List ordered merging (non-tail-recursive)
(define (merge l1 l2)
  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    ((< (car l1) (car l2))
     (cons (car l1) (merge (cdr l1) l2)))
    (else
     (cons (car l2) (merge l1 (cdr l2))))))


;; 5. List ordered merging (tail-recursive)
(define (merge-tail l1 l2)
  (let rec ([acc '()]
            [list1 l1]
            [list2 l2])
    (if (empty? list1)
        (if (empty? list2)
            (reverse acc)
            (rec (cons (first list2) acc) list1 (rest list2)))
        (if (empty? list2)
            (rec (cons (first list1) acc) (rest list1) list2)
            (if (> (first list2) (first list1))
                (rec (cons (first list1) acc) (rest list1) list2)
                (rec (cons (first list2) acc) list1 (rest list2)))))))


;; 6. List run-length encoding
(define (run-length-encode lst)
  (let rec ([acc '()]
            [counter 1]
            [list lst])
    (if (empty? list)
        (reverse acc)
        (if (empty? (rest list))
            (rec (cons (cons (first list) (cons counter '())) acc) 0 (rest list))
            (if (equal? (first list) (first (rest list)))
                (rec acc (+ counter 1) (rest list))
                (rec (cons (cons (first list) (cons counter '())) acc) 1 (rest list)))))))


;; 7. List run-length decoding
(define (run-length-decode lst)
  (let rec ([acc '()]
            [restlists lst]
            [counter (first (rest (first lst)))]
            [currentList (first lst)])
    (if (empty? restlists)
        (reverse acc)
        (if (equal? counter 1)
            (if (empty? (rest restlists))
                (rec (cons (first currentList) acc) '() 0 currentList)
                (rec (cons (first currentList) acc) (rest restlists) (first (rest (first (rest restlists)))) (first (rest restlists))))
            (rec (cons (first currentList) acc) restlists (+ counter -1) currentList)))))
                 

;; 8. Labeling sexps
(define (label-sexp sexp)
    (match sexp
      [(? integer?)
       `(int ,sexp)]

      [(? symbol?)
       `(var ,sexp)]

      [(list (and op (or '+ '* '/ '-)) lhs rhs)
       `(arith (op ,op) ,( label-sexp lhs) ,(label-sexp rhs))]

      [(list (and ? symbol?) (? symbol? variable))
       `(funcall (name ,symbol?) (var ,variable))]
      
      [(list (and ? symbol?) (? integer? val))
       `(funcall (name ,symbol?) (int ,val))]

      [(list (and ? symbol?) (list (and operator (or '+ '* '/ '-)) lhs rhs))
       `(funcall (name ,symbol?) (arith (op ,operator) ,(label-sexp lhs) ,(label-sexp rhs)))]

      [_ (error "Unable to label!")]))
      