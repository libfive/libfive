(define-module (ao shapes))

(use-modules (ao overload))

(define-public (square x) (* x x))
(define-public (circle x0 y0 r)
        (lambda (x y z) (- (sqrt (+ (square (- x x0))
                                    (square (- y y0)))) r)))
(define-public (rect xmin xmax ymin ymax)
        (lambda (x y z) (max (- xmin x) (- x xmax)
                             (- ymin y) (- y ymax))))

;; CSG operations
(define-public (union . shapes)
        (lambda (x y z) (apply min (map (lambda (s) (s x y z)) shapes))))
(define-public (intersection . shapes)
        (lambda (x y z) (apply max (map (lambda (s) (s x y z)) shapes))))
(define-public (difference . shapes)
        (let ((accum (apply union (cdr shapes))))
        (lambda (x y z) (max ((car shapes) x y z)
                             (-(accum x y z))))))
;; Transforms
(define-public (move f dx dy)
        (lambda (x y) (f (+ x dx) (+ y dy))))
