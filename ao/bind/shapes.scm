(use-modules (ao kernel) (ao vec) (ao csg) (ao transforms))

(define-public (circle center r)
    "circle #[x y] r"
    (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y))) r))
        center))

(define-public (sphere center r)
    "sphere #[x y z] r"
    (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y) (square z))) r))
        center))

(define-public (rectangle a b)
    "rectangle #[xmin ymin] #[xmax ymax]
    Constructs a rectangle from its corners"
    (lambda-shape (x y z)
        (max (- (.x a) x) (- x (.x b)) (- (.y a) y) (- y (.y b)))))

(define-public (box a b)
    "box #[xmin ymin zmin] #[xmax ymax zmax]\\"
    (extrude-z (rectangle a b) (.z a) (.z b)))

(define-public (extrude-z shape zmin zmax)
    "extrude-z shape zmin zmax
    Extrudes a 2D shape between za and zb"
    (max shape (lambda-shape (x y z) (max (- zmin z) (- z zmax)))))

(define-public (cylinder center r zmin zmax)
    "cylinder #[x0 y0] r zmin zmax
    A cylinder (oriented along the Z axis) "
    (extrude-z (circle center r) zmin zmax))

(define-public (triangle a b c)
  "triangle #[x0 y0] #[x1 y1] #[x2 y2]
  Returns a 2D triangle"
  (define (half-plane a b)
    (lambda-shape (x y z)
      (- (* (- (.y b) (.y a)) (- x (.x a)))
         (* (- (.x b) (.x a)) (- y (.y a))))))
  (if (< 0 (.z (cross (vec3 (- b a) 0) (vec3 (- c a) 0))))
    (intersection
      (half-plane a b) (half-plane b c) (half-plane c a))
    (intersection
      (half-plane a c) (half-plane c b) (half-plane b a))))
