(define-module (ao shapes))

(use-modules (ao overload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2D shapes
(define-public (circle center r)
    " Constructs a circle from a center '(x0 y0 z0) and radius "
    (move (lambda (x y z) (- (sqrt (+ (* x x) (* y y))) r))
          center))

(define-public (rectangle a b)
    " Constructs a rectangle from two '(x y) lists representing corners "
    (let* ((xa (car a))
           (xb (car b))
           (ya (cadr a))
           (yb (cadr b))
           (xmin (min xa xb))
           (xmax (max xa xb))
           (ymin (min ya yb))
           (ymax (max ya yb)))
    (lambda (x y z) (max (- xmin x) (- x xmax)
                         (- ymin y) (- y ymax)))))

(define-public (triangle a b c)
    " Constructs a triangle from three '(x y) lists"
    (let* ;; Find the center point of the triangle
          ((xm (/ (+  (car a)  (car b)  (car c)) 3))
           (ym (/ (+ (cadr a) (cadr b) (cadr c)) 3))

           ;; Calculate the angles of each point about the center
           (get-angle (lambda (p) (atan (- (car p) xm) (- (cadr p) ym))))

           ;; Extract the three angles
           (ta (get-angle a))
           (tb (get-angle b))
           (tc (get-angle c))

           ;; Rotate the points so that the smallest angle is first
           (sorted (cond ((and (< tb ta) (< tb tc)) (list tb tc ta))
                         ((and (< tc ta) (< tc tb)) (list tc ta tb))
                         (else (list ta tb tc))))

           ;; If the points are in increasing order, it's a sign that
           ;; they aren't sorted in a clockwise fashion.  In this case, swap
           ;; the second and third point to make them clockwise
           (clockwise (if (> (caddr sorted) (cadr sorted))
                          (list a c b) (list a b c)))

           ;; Extract coordinates from sorted list
           (x0 (caar clockwise))   (y0 (cdar clockwise))
           (x1 (caadr clockwise))  (y1 (cdadr clockwise))
           (x2 (caaddr clockwise)) (y2 (cdaddr clockwise))

           ;; Function to calculate one edge of the triangle
           (edge (lambda (x0 y0 dx dy x y)
                    (- (* dy (- x x0)) (* dx (- y y0))))))

          (lambda (x y z) (max (edge x2 y2 (- x0 x2) (- y0 y2) x y)
                               (edge x1 y1 (- x2 x1) (- y2 y1) x y)
                               (edge x0 y0 (- x1 x0) (- y1 y0) x y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2D -> 3D functions

(define-public (extrude-z shape za zb)
    "Extrudes the given shape between za and zb"
    (let ((zmin (min za zb))
          (zmax (max za zb)))
    (lambda (x y z) (max (shape x y z) (- zmin z) (- z zmax)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3D shapes

(define-public (sphere center r)
    "Defines a sphere from a center '(x0 y0 z0) and radius r"
    (move (lambda (x y z) (- (sqrt (+ (* x x) (* y y) (* z z))) r))
          center))

(define-public (cube a b)
    "Defines a cube with corners a and b, each a list '(x y z)"
    (extrude-z (rectangle a b) (caddr a) (caddr b)))

(define-public (cylinder-z base r height)
    "Creates a circle with the given base '(x y z), radius r, and height dz"
    (let ((zmin (caddr base)))
    (extrude-z (circle base r) zmin (+ zmin height))))

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

(define-public (blend a b m)
    "Blends the given shape by a certain amount"
    (let ((joint (union a b))
          (fillet (lambda (x y z)
                    (- (+ (sqrt (abs (a x y z)))
                          (sqrt (abs (b x y z)))) m))))
    (union joint fillet)))

(define-public (morph a b m)
    "Morphs between two shapes.  m = 0 produces a, m = 1 produces b"
    (lambda (x y z) (+ (* (a x y z) (- 1 m)) (* (b x y z) m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transforms
(define-public (move f delta)
    "Moves a shape by delta '(dx dy [dz=0])"
    (let ((dx   (car delta))
          (dy  (cadr delta))
          (dz (if (>= (length delta) 3) (caddr delta) 0)))
    (lambda (x y z) (f (- x dx) (- y dy) (- z dz)))))

(define-public (rotate-z shape angle . args)
    "Rotate the given shape by an angle in radians
The center of rotation is 0,0 or specified by optional argument '(x0 y0)"
    (let* ((argc (length args))
           (x0 (if (> argc 0) (caar  args) 0))
           (y0 (if (> argc 0) (cadar args) 0))
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

(define-public (scale-x shape sx . args)
    "Scales a shape by sx on the x axis about 0 or an optional argument x0"
    (let ((x0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape (+ x0 (/ (- x x0) sx)) y z))))

(define-public (scale-y shape sy . args)
    "Scales a shape by sy on the y axis about 0 or an optional argument y0"
    (let ((y0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape x (+ y0 (/ (- y y0) sy)) z))))

(define-public (scale-z shape sz . args)
    "Scales a shape by sz on the z axis about 0 or an optional argument z0"
    (let ((z0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape x y (+ z0 (/ (- z z0) sz))))))

(define-public (shear-x-y shape ymin ymax dx0 dx1)
    "Shears a shape in the XY plane"
    (lambda (x y z)
        (let ((f (/ (- y ymin) (- ymax ymin))))
        (shape (- x (* dx0 (- 1 f)) (* dx1 f)) y z))))

(define-public (taper-x-y shape x0 ymin ymax s0 s1)
    "Tapers a shape in the XY plane"
    (let ((dy (- ymax ymin))
          (ds (- s1 s0)))
    (lambda (x y z)
        (let ((s (/ dy (- (* s1 (- y ymin)) (* s0 (- y ymax))))))
        (shape (+ x0 (* (- x x0) s))
               y z)))))

(define-public (taper-xy-z shape xy zmin zmax s0 s1)
    "Tapers a shape along the Z axis"
    (let ((x0 (car xy))
          (y0 (cadr xy))
          (dz (- zmax zmin)))
    (lambda (x y z)
        (let ((s (/ dz (- (* s1 (- z zmin)) (* s0 (- z zmax))))))
        (shape (+ x0 (* (- x x0) s))
               (+ y0 (* (- y y0) s))
               z)))))
