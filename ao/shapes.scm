(define-module (ao shapes))

(use-modules (ao overload))

(define-public pi 3.1415926)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2D shapes
(define-public (circle center r)
    "circle '(x y) r
    Constructs a circle from a center and radius"
    (move (lambda (x y z) (- (sqrt (+ (square x) (square y))) r))
          center))

(define-public (rectangle a b)
    "rectangle '(xmin ymin) '(xmax ymax)
    Constructs a rectangle from two lists representing its corners"
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

(define-public (rounded-rectangle a b r)
    "rounded-rectangle '(xmin ymin) '(xmax ymax) r
    Constructs a rounded rectangle from two  lists representing corners
    and a value r in the range 0 to 1"
    (let* ((xa (car a))
           (xb (car b))
           (ya (cadr a))
           (yb (cadr b))
           (xmin (min xa xb))
           (xmax (max xa xb))
           (ymin (min ya yb))
           (ymax (max ya yb))
           (r (* r (min (- xmax xmin) (- ymax ymin)) 0.5)))
    (union (rectangle (list xmin (+ ymin r)) (list xmax (- ymax r)))
           (rectangle (list (+ xmin r) ymin) (list (- xmax r) ymax))
           (circle (list (+ xmin r) (+ ymin r)) r)
           (circle (list (+ xmin r) (- ymax r)) r)
           (circle (list (- xmax r) (+ ymin r)) r)
           (circle (list (- xmax r) (- ymax r)) r))))


(define-public (triangle a b c)
    "triangle '(xa ya) '(xb yb) '(xc yc)
    Constructs a triangle from three corners "
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
           (x0 (caar clockwise))   (y0 (cadr (car clockwise)))
           (x1 (caadr clockwise))  (y1 (cadr (cadr clockwise)))
           (x2 (caaddr clockwise)) (y2 (cadr (caddr clockwise)))

           ;; Function to calculate one edge of the triangle
           (edge (lambda (x0 y0 dx dy)
                 (lambda (x y z)
                    (- (* dy (- x x0)) (* dx (- y y0)))))))

          (intersection (edge x2 y2 (- x0 x2) (- y0 y2))
                        (edge x1 y1 (- x2 x1) (- y2 y1))
                        (edge x0 y0 (- x1 x0) (- y1 y0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2D -> 3D functions

(define-public (extrude-z shape za zb)
    "extrude-z shape za zb
    Extrudes the given 2D shape between za and zb"
    (let ((zmin (min za zb))
          (zmax (max za zb)))
    (lambda (x y z) (max (shape x y z) (- zmin z) (- z zmax)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3D shapes

(define-public (sphere center r)
    "sphere '(x y z) r
    Defines a sphere from a center and radius"
    (move (lambda (x y z) (- (sqrt (+ (square x) (square y) (square z))) r))
          center))

(define-public (cube a b)
    "cube '(xmin ymin zmin) '(xmax ymax zmax)
    Defines a cube from its two corners"
    (extrude-z (rectangle a b) (caddr a) (caddr b)))

(define-public (cylinder-z base r height)
    "cylinder '(x y zmin) r dz
    Creates a circle from a base location, radius, and height"
    (let ((zmin (caddr base)))
    (extrude-z (circle base r) zmin (+ zmin height))))

(define-public (cone-z base r height)
    "cone '(x y zmin) r dz
    Creates a cone from a base location, radius, and height"
    (let ((zmin (caddr base)))
    (taper-xy-z (cylinder-z base r height)
                base zmin (+ zmin height) 1 0)))

(define-public (pyramid-z a b zmin height)
    "cone '(xmin ymin) '(xmax ymax) zmin dz
    Creates a pyramid from a base rectangle, lower z value and height"
    (taper-xy-z (extrude-z (rectangle a b) zmin (+ zmin height))
        (list (/ (+  (car a)  (car b)) 2)
              (/ (+ (cadr a) (cadr b)) 2))
        zmin (+ zmin height) 1 0))

(define-public (torus-z center R r)
    "torus '(x y z) R r
    Create a torus from the given center, outer radius, and inner radius"
    (let ((distance (lambda (a b)
        (sqrt (+ (* a a) (* b b))))))
    (move (lambda (x y z)
        (let ((d (distance x y)))
        (- (distance (- R d) z) r)))
    center)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CSG operations
(define-public (union . shapes)
    "union a [b [c [...]]]
    Returns the union of any number of shapes"
    (if (= 0 (length shapes))
        (error "Cannot take the union of an empty list")
        (lambda (x y z) (apply min (map (lambda (s) (s x y z)) shapes)))))

(define-public (intersection . shapes)
    "intersection a [b [c [...]]]
    Returns the intersection of any number of shapes"
    (if (= 0 (length shapes))
        (error "Cannot take the union of an empty list")
        (lambda (x y z) (apply max (map (lambda (s) (s x y z)) shapes)))))

(define-public (difference a . b)
    "difference a b [c [d [...]]]
    Subtracts any number of shapes from the first argument"
    (let ((accum (apply union b)))
    (lambda (x y z) (max (a x y z)
                         (-(accum x y z))))))

(define-public (offset s o)
    "offset shape o
    Expand or contract a given shape by an offset"
    (lambda (x y z) (- (s x y z) o)))

(define-public (clearance a b o)
    "clearance a b o
    Expands shape b by the given offset then subtracts it from shape a"
    (let ((bo (offset b o)))
    (lambda (x y z) (max (a x y z) (- (bo x y z))))))

(define-public (shell shape o)
    "shell shape o
    Returns a shell of a shape with the given offset"
    (clearance shape shape o))

(define-public (blend a b m)
    "blend a b m
    Blends two shapes by the given amount"
    (let ((joint (union a b))
          (fillet (lambda (x y z)
                    (- (+ (sqrt (abs (a x y z)))
                          (sqrt (abs (b x y z)))) m))))
    (union joint fillet)))

(define-public (morph a b m)
    "morph a b m
    Morphs between two shapes.
    m = 0 produces a, m = 1 produces b"
    (lambda (x y z) (+ (* (a x y z) (- 1 m)) (* (b x y z) m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transforms
(define-public (move shape delta)
    "move shape '(dx dy [dz=0])
    Moves a shape by the given offset"
    (let ((dx   (car delta))
          (dy  (cadr delta))
          (dz (if (>= (length delta) 3) (caddr delta) 0)))
    (lambda (x y z) (shape (- x dx) (- y dy) (- z dz)))))

(define-public (rotate-x shape angle . args)
    "rotate-x shape angle ['(x0 y0 z0)]
    Rotate the given shape by an angle in radians
    The center of rotation is 0,0,0 or specified by the optional argument"
    (let* ((argc (length args))
           (y0 (if (> argc 1) (cadar  args) 0))
           (z0 (if (> argc 2) (caddar args) 0))
           (ca (cos angle))
           (sa (sin angle)))
    (move (lambda (x y z)
        ((move shape (list 0 (- y0) (- z0)))
            x
            (+ (* ca y) (* sa z))
            (+ (* (- sa) y) (* ca z))
            ))
        (list 0 y0 z0))))

(define-public (rotate-y shape angle . args)
    "rotate-y shape angle ['(x0 y0 z0)]
    Rotate the given shape by an angle in radians
    The center of rotation is 0,0,0 or specified by the optional argument"
    (let* ((argc (length args))
           (x0 (if (> argc 0) (caar  args) 0))
           (z0 (if (> argc 2) (caddar args) 0))
           (ca (cos angle))
           (sa (sin angle)))
    (move (lambda (x y z)
        ((move shape (list (- x0) 0 (- z0)))
            (+ (* ca x) (* sa z))
            y
            (+ (* (- sa) x) (* ca z))
            ))
        (list x0 0 z0))))

(define-public (rotate-z shape angle . args)
    "rotate-z shape angle ['(x0 y0 z0)]
    Rotate the given shape by an angle in radians
    The center of rotation is 0,0,0 or specified by the optional argument"
    (let* ((argc (length args))
           (x0 (if (> argc 0) (caar  args) 0))
           (y0 (if (> argc 1) (cadar args) 0))
           (ca (cos angle))
           (sa (sin angle)))
    (move (lambda (x y z)
        ((move shape (list (- x0) (- y0)))
            (+ (* ca x) (* sa y))
            (+ (* (- sa) x) (* ca y))
            z))
        (list x0 y0))))

(define-public (reflect-x shape . args)
    "reflect-x shape [x0]
    Reflect the given shape about the x origin or an optional offset"
    (let ((x0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape (- (* 2 x0) x) y z))))

(define-public (reflect-y shape . args)
    "reflect-y shape [y0]
    Reflect the given shape about the y origin or an optional offset"
    (let ((y0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape x (- (* 2 y0) y) z))))

(define-public (reflect-z shape . args)
    "reflect-z shape [z0]
    Reflect the given shape about the z origin or an optional offset"
    (let ((z0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape x y (- (* 2 z0) z)))))

(define-public (reflect-xy shape)
    "reflect-xy shape
    Reflects the given shape about the line x = y"
    (lambda (x y z) (shape y x z)))

(define-public (reflect-xz shape)
    "reflect-xz shape
    Reflects the given shape about the line x = z"
    (lambda (x y z) (shape z y x)))

(define-public (reflect-yz shape)
    "reflect-yz shape
    Reflects the given shape about the line y = z"
    (lambda (x y z) (shape x z y)))

(define-public (scale-x shape sx . args)
    "scale-x shape sx [x0]
    Scales a shape by sx on the x axis about 0 or an optional offset"
    (let ((x0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape (+ x0 (/ (- x x0) sx)) y z))))

(define-public (scale-y shape sy . args)
    "scale-y shape sy [y0]
    Scales a shape by sy on the y axis about 0 or an optional offset"
    (let ((y0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape x (+ y0 (/ (- y y0) sy)) z))))

(define-public (scale-z shape sz . args)
    "scale-z shape sz [z0]
    Scales a shape by sz on the z axis about 0 or an optional offset"
    (let ((z0 (if (> (length args) 0) (car args) 0)))
    (lambda (x y z) (shape x y (+ z0 (/ (- z z0) sz))))))

(define-public (shear-x-y shape ymin ymax dx0 dx1)
    "shear-x-y shape ymin ymax dx0 dx1
    Shears a shape in the XY plane.
    dx0 is the offset at ymin; dx1 is the offset at ymax"
    (lambda (x y z)
        (let ((f (/ (- y ymin) (- ymax ymin))))
        (shape (- x (* dx0 (- 1 f)) (* dx1 f)) y z))))

(define-public (taper-x-y shape x0 ymin ymax s0 s1)
    "taper-x-y shape x0 ymin ymax s0 s1
    Tapers a shape in the XY plane.
    s0 is the scale at ymin; s1 is the scale at ymax
    Scales are applied centered at x0"
    (let ((dy (- ymax ymin))
          (ds (- s1 s0)))
    (lambda (x y z)
        (let ((s (/ dy (- (* s1 (- y ymin)) (* s0 (- y ymax))))))
        (shape (+ x0 (* (- x x0) s))
               y z)))))

(define-public (taper-xy-z shape xy zmin zmax s0 s1)
    "taper-xy-z shape '(x0 y0) ymin ymax s0 s1
    Tapers a shape in the along the Z axis.
    s0 is the scale at zmin; s1 is the scale at zmax
    Scales are applied centered at x0, y0 in the XY plane"
    (let ((x0 (car xy))
          (y0 (cadr xy))
          (dz (- zmax zmin)))
    (lambda (x y z)
        (let ((s (/ dz (- (* s1 (- z zmin)) (* s0 (- z zmax))))))
        (shape (+ x0 (* (- x x0) s))
               (+ y0 (* (- y y0) s))
               z)))))

(define-public (array-2d shape i j dx dy)
    "array-2d shape i j dx dy
    Iterates a part in a 2D array.
    i and j are part counts along each axis
    dx and dy are offsets along each axis"
    (let ((a (apply union
        (map (lambda (i)
            (move shape (list (* i dx) 0 0))) (iota i)))))
    (apply union
        (map (lambda (j)
            (move a (list 0 (* j dy) 0))) (iota j)))))

(define-public (array-3d shape i j k dx dy dz)
    "array-3d shape i j k dx dy dz
    Iterates a part in a 3D array.
    i, j, k are part counts along each axis
    dx, dy, dz are offsets along each axis"
    (let*
        ((a (apply union
        (map (lambda (i)
            (move shape (list (* i dx) 0 0))) (iota i))))
        (b (apply union
        (map (lambda (j)
            (move a (list 0 (* j dy) 0))) (iota j)))))
    (apply union
        (map (lambda (k)
            (move b (list 0 0 (* k dz)))) (iota k)))))

(define-public (array-polar shape n . args)
    "array-polar shape n ['(x y)]
    Iterate a shape about an optional center position"
    (apply union
        (map (lambda (i)
            (apply rotate-z (append (list shape (* 2 pi (/ i n))) args)))
            (iota n))))
