(use-modules (ao kernel))

(define-public (union . args)
  "union a [b [c [...]]]
  Returns the union of any number of shapes"
  (apply min args))

(define-public (intersection . args)
  "intersection a [b [c [...]]]
  Returns the intersection of any number of shapes"
  (apply max args))

(define-public (inverse a)
  "inverse a
  Returns a shape that's the inverse of the input shape"
  (- a))

(define-public (difference a . bs)
  "difference a b [c [d [...]]]
  Subtracts any number of shapes from the first argument"
  (intersection a (inverse (apply union bs))))

(define-public (offset s o)
  "offset shape o
  Expand or contract a given shape by an offset"
  (+ s o))

(define-public (clearance a b o)
  "clearance a b o
  Expands shape b by the given offset then subtracts it from shape a"
  (difference a (offset b o)))

(define-public (shell shape o)
  "shell shape o
  Returns a shell of a shape with the given offset"
  (clearance shape shape o))

(define-public (blend a b m)
  "blend a b m
  Blends two shapes by the given amount"
  (union a b (- (+ (sqrt (abs a)) (sqrt (abs b))) m)))

(define-public (morph a b m)
  "morph a b m
  Morphs between two shapes.
  m = 0 produces a, m = 1 produces b"
  (+ (* a (- 1 m)) (* b m)))

(define-public (loft a b zmin zmax)
  "loft a b zmin zmax
  Produces a blended loft between a (at zmin) and b (at zmax)
  a and b should be 2D shapes (i.e. invariant along the z axis) "
  (lambda-shape (x y z)
    (max (- z zmax) (- zmin z)
      (/ (+ (* (- z zmin) b) (* (- zmax z) a))
         (- zmax zmin)))))
