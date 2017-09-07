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
