(define (taper-z shape zmin zmax)
  (define (scale z) (/ (- z zmax) (- zmax zmin)))
  (remap-shape (shape x y z)
    (/ x (scale z))
    (/ y (scale z))
    z))

(taper-z (sphere 1) -1 1.1)

(set-quality! 8)
(set-resolution! 60)
(set-bounds! [-10 -10 -10] [10 10 10])