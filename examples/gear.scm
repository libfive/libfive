;; INCOMPLETE!
(use-modules (ao shapes))

;; The involute curve can be thought of as the path traced by the tip of a
;; string of length 2 * pi as it is unwound from around a unit circle.
;;
;; For a point p at x,y, we want to find the point q such that pq is tangent
;; to the unit circle.  Our imaginary string wraps clockwise around the circle
;; from (1,0) until q, then passes in a straight line to p.
;;
;; The point p is inside the involute if the total length (winding clockwise
;; from 0 to q then straight from q to p) is under 2 pi.
(define (involute x y z)
    (let* ((theta (atan y x))
           (r (sqrt (+ (square x) (square y))))
           (qpo (asin (/ 1 r))) ; One of the triangle angles, by the sine rule
           (poq (- (/ pi 2) qpo)) ; The other triangle angles, since sum = pi
           (xoq (+ theta poq))  ; Angle from x axis to point on unit circle
           (dist (sqrt (- (square r) 1))) ;; Distance pq (Pythagoras)
           (inside (- dist xoq))) ; Negative if inside, else positive or nan
    (max (- y) (nan-fill inside  ;; Use nan-fill to fix negative sqrts
                         (- ((circle '(0 0) 1) x y z))))))

(define count 64)
(define angle (/ (* 2 pi) count))

(define tooth
    (let* ((ir (rotate-z involute (- angle)))
           (clipped (intersection ir (lambda (x y z) y))))
    (union clipped (reflect-y clipped))))

(define teeth (apply union (map
    (lambda (i) (rotate-z tooth (* i angle)))
    (iota count))))

;(ao-show involute)

;(define t (extrude-z teeth 0 0.1))
(ao-show teeth)
