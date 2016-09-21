(define (square i) (* i i))

(define (mandelbrot depth)
  (lambda (x y z)
    (let recurse ((real 0) (imag 0) (i depth))
      (if (> i 0)
        (recurse (+ (- (square real) (square imag)) x)
                 (+ (* 2 real imag) y)
                 (- i 1))
        (+ (square real) (square real) -4)))))

(ao-show "mandelbrot" (mandelbrot 10))
