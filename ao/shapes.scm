(define-module (ao shapes))

(use-modules (ao jit))

(define-public (square x) (* x x))
(define-public (circle x0 y0 r)
        (lambda (x y) (- (sqrt (+ (square (- x x0))
                                  (square (- y y0)))) r)))
(define-public (rect xmin xmax ymin ymax)
        (lambda (x y) (max (- xmin x) (- x xmax)
                           (- ymin y) (- y ymax))))
(define-public (union . shapes)
        (lambda (x y) (apply min (map (lambda (s) (s x y)) shapes))))
