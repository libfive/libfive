(define-module (ao shapes))

(use-modules (ao overload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

(define-public (circle x0 y0 r)
        (lambda (x y z) (- (sqrt (+ (square (- x x0))
                                    (square (- y y0)))) r)))
(define-public (rect xmin xmax ymin ymax)
        (lambda (x y z) (max (- xmin x) (- x xmax)
                             (- ymin y) (- y ymax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CSG operations
(define-public (union . shapes)
    "Returns the union of a set of shapes"
    (lambda (x y z) (apply min (map (lambda (s) (s x y z)) shapes))))

(define-public (intersection . shapes)
    "Returns the intersection of a set of shapes"
    (lambda (x y z) (apply max (map (lambda (s) (s x y z)) shapes))))

(define-public (difference a . b)
    "Subtract a set of shapes from a target shape"
    (let ((accum (apply union b)))
    (lambda (x y z) (max (a x y z)
                         (-(accum x y z))))))

(define-public (offset s o)
    "Expand or contract a given shape s by an offset o"
    (lambda (x y z) (- (s x y z) o)))

(define-public (clearance a b o)
    "Expands shape b by offset o then subtracts it from shape a"
    (let ((bo (offset b o)))
    (lambda (x y z) (max (a x y z) (- (bo x y z))))))

(define-public (shell a o)
    "Returns a shell of shape a offset by amount o"
    (clearance a a o))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transforms
(define-public (move f dx dy dz)
        (lambda (x y z) (f (+ x dx) (+ y dy) (+ z dz))))

(define-public (rotate-z shape angle . args)
    "Rotate the given shape by an angle in radians
The center of rotation is 0,0 or specified by optional arguments x0, y0"
    (let* ((argc (length args))
           (x0 (if (> argc 0) (car  args) 0))
           (y0 (if (> argc 1) (cadr args) 0))
           (ca (cos angle))
           (sa (sin angle)))
    (lambda (x y z) (shape (+ (* ca x) (* sa y))
                           (+ (* (- sa) x) (* ca y))
                           z))))

(define-public (reflect-x shape . args)
    "Reflect the given shape about the x origin or an optional argument x0"
    (let ((x0 (if (> 0 (length args)) (car args) 0)))
    (lambda (x y z) (shape (- (* 2 x0) x) y z))))

(define-public (reflect-y shape . args)
    "Reflect the given shape about the y origin or an optional argument y0"
    (let ((y0 (if (> 0 (length args)) (car args) 0)))
    (lambda (x y z) (shape x (- (* 2 y0) y) z))))

(define-public (reflect-z shape . args)
    "Reflect the given shape about the z origin or an optional argument z0"
    (let ((z0 (if (> 0 (length args)) (car args) 0)))
    (lambda (x y z) (shape x y (- (* 2 z0) z)))))
