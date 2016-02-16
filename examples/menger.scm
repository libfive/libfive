;; Menger sponge fractal
(use-modules (ao shapes))

(define (recurse x y scale i)
    (let* ((s (/ scale 2))
           (this (rectangle (list (- x s) (- y s))
                            (list (+ x s) (+ y s)))))
    (if (= i 0)
        this
        (let ((j (- i 1))
              (t (/ scale 3)))
            (union this (recurse (+ x scale) y t j) (recurse (- x scale) y t j)
                        (recurse x (+ y scale) t j) (recurse x (- y scale) t j)
                        (recurse (+ x scale) (+ y scale) t j)
                        (recurse (+ x scale) (- y scale) t j)
                        (recurse (- x scale) (+ y scale) t j)
                        (recurse (- x scale) (- y scale) t j))))))

;; Increase this value to make the sponge more intricate
(define iterations 2)

(define menger-2d (recurse 0 0 1 iterations))
(define menger-3d (difference (cube '(-1.5 -1.5 -1.5) '(1.5 1.5 1.5))
                               menger-2d (reflect-xz menger-2d)
                                         (reflect-yz menger-2d)))
(ao-show menger-3d)
