(define (twisted-strip-xz len width thickness)
  (define c (cube (list (/ width -2) (/ len -2) (/ thickness -2))
                  (list (/ width  2) (/ len  2) (/ thickness  2))))
  (lambda (x y z)
    (let* ((frac (+ 0.5 (/ y len))) ; 0 to 1
           (a  (atan z x)) ; -pi to pi
           (r  (sqrt (+ (* z z) (* x x))))
           (a_ (+ a (* frac pi)))
           (x_ (* r (cos a_)))
           (z_ (* r (sin a_))))
      (c x_ y z_))))

(define (map-polar-xy shape r)
  (lambda (x y z)
    (let* ((a  (atan y x)) ; -pi to pi
           (r_ (sqrt (+ (* y y) (* x x))))
           (x_ (- r_ r))
           (y_ (* a r)))
      (shape x_ y_ z))))

(define (mobius radius width thickness)
  (define twist (twisted-strip-xz (* 2.01 pi radius) width thickness))
  (map-polar-xy twist radius))

(ao-show "mobius" (mobius 5 1 0.5))
