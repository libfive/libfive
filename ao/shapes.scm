(define-module (ao shapes))

(use-modules (ao overload))

(define-public (square x) (* x x))
(define-public (circle x0 y0 r)
        (lambda (x y) (- (sqrt (+ (square (- x x0))
                                  (square (- y y0)))) r)))
(define-public (rect xmin xmax ymin ymax)
        (lambda (x y) (max (- xmin x) (- x xmax)
                           (- ymin y) (- y ymax))))

;; CSG operations
(define-public (union . shapes)
        (lambda (x y) (apply min (map (lambda (s) (s x y)) shapes))))
(define-public (intersection . shapes)
        (lambda (x y) (apply max (map (lambda (s) (s x y)) shapes))))

;; Transforms
(define-public (move f dx dy)
        (lambda (x y) (f (+ x dx) (+ y dy))))
