(define (recurse center scale i)
  (define base
    (rectangle (- center scale) (+ center scale)))
  (if (= i 0)
    base
    (apply union (cons base
      (map (lambda (o)
        (recurse (+ center (* 2 o scale))
                 (/ scale 3)
                 (1- i)))
          (list
            #[1 0] #[-1 0]
            #[0 1] #[0 -1]
            #[1 1] #[1 -1]
            #[-1 1] #[-1 -1]
          ))))
))

;; Build the 2D cutout
(define cutout (recurse #[0 0] 2/3 2))

;; Then reflect and cut it from a cube on all axes
(difference (box #[-2 -2 -2] #[2 2 2])
    cutout
    (reflect-yz cutout)
    (reflect-xz cutout))

(set-quality! 8)
(set-resolution! 10)
(set-bounds! [-10 -10 -10] [10 10 10])