(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (ao shapes) (ao transforms) (ao csg) (ao user))

;;;
;;; L-System: Pythagoras tree
;;;
;;; cf. https://en.wikipedia.org/wiki/L-system#Example_2:_Pythagoras_tree
;;;


(define unit 10)
(define segment (rotate-x (cylinder-z '(0 0 0) 5 unit) (/ pi -2)))

(define (recurse grammar seed iteration)
  (if (zero? iteration)
      seed
      (recurse grammar (grammar seed) (1- iteration))))

(define (grammar seq)
  (let loop ((seq seq))
    (match seq
      (() '())
      ((1 . rest) (cons* 1 1 (loop rest)))
      ((0 . rest) (cons* 1 (list 0) 0 (loop rest)))
      ((head . tail) (cons (grammar head) (loop tail))))))

(define (vm seq)
  (match seq
    ((a) segment)
    ((head tail ...)
     (match head
       (0 (union segment (move (vm tail) (list 0 unit 0))))
       (1 (union segment (move (vm tail) (list 0 unit 0))))
       (_ (union (rotate-z (vm head) (/ pi 4)) (rotate-z (vm tail) (- (/ pi 4)))))))))


(define tree (vm (pk (recurse grammar '(0) 3))))

(ao-show tree)
