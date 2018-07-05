;; Distance functions can be created with
;; lambda-shape and define-shape, which
;; have similar syntax as their Scheme
;; equivalents but return a math tree.
(define (sphere r)
  (lambda-shape (x y z)
    (- (sqrt (+ (* x x)
                (* y y)
                (* z z))) r)))

;; Numbers that are prefixed by a octothorpe
;; (#) are interpreted as free variables.
;; Dragging the model's surface in the 3D
;; viewport will automatically update the value
;; of the variable in the script.
(define-shape (cutout x y z)
  (let ((r #0.2))
  (max (- r (abs x))
       (- r (abs y))
       (- r (abs z)))))

;; There's not always a valid solution!
;; The inner sphere's radius isn't free in the
;; code below; if you drag it, you should see
;; the model turn red, indicating that no
;; solution was found.
(max (sphere #1)
     (- cutout)
     (- (sphere 0.8)))

(set-quality! 8)
(set-resolution! 10)
(set-bounds! [-10 -10 -10] [10 10 10])