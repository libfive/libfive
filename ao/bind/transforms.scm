(use-modules (ao kernel))

(define-public (move shape delta)
    "move shape #(dx dy [dz])
    Moves the given shape in 2D or 3D space"
    (remap-shape (shape x y z)
        (- x (.x delta))
        (- y (.y delta))
        (- z (catch #t (lambda ()(.z delta)) (lambda (. _) 0)))))

(define-public (reflect-xy shape)
    "reflect-xy shape
    Moves the given shape across the plane Y=X"
    (remap-shape (shape x y z) y x z))

(define-public (reflect-yz shape)
    "reflect-xy shape
    Moves the given shape across the plane Y=Z"
    (remap-shape (shape x y z) x z y))

(define* (rotate-x shape angle #:optional center)
    "rotate-x shape angle [#(x0 y0 z0)]
    Rotate the given shape by an angle in radians
    The center of rotation is 0,0,0 or specified by the optional argument"
    (when (not center) (set! center #(0 0 0)))
    (let ((centered (move shape (list (- (.x center))
                                      (- (.y center))
                                      (- (.z center)))))
          (ca (cos angle))
          (sa (sin angle)))
      (move (remap-shape (centered x y z)
            x
            (+ (* ca y) (* sa z))
            (+ (* (- sa) y) (* ca z))) center)))
(export rotate-x)
