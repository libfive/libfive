(use-modules (oop goops) (ice-9 textual-ports))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <vec2> () x y)

(define-method (vec2 (x <number>) (y <number>))
  (let ((v (make <vec2>)))
    (slot-set! v 'x x)
    (slot-set! v 'y y)
    v))
(export vec2)

(define-method (write (self <vec2>) port)
  (format port "#[~a ~a]" (slot-ref self 'x) (slot-ref self 'y)))

(define-method (+ (a <vec2>) (b <vec2>))
  (vec2 (+ (slot-ref a 'x) (slot-ref b 'x))
        (+ (slot-ref a 'y) (slot-ref b 'y))))
(define-method (+ (a <vec2>) (b <number>))
  (vec2 (+ (slot-ref a 'x) b)
        (+ (slot-ref a 'y) b)))
(define-method (+ (a <number>) (b <vec2>)) (+ b a))

(define-method (* (a <vec2>) (b <number>))
  (vec2 (* (slot-ref a 'x) b)
        (* (slot-ref a 'y) b)))
(define-method (* (a <number>) (b <vec2>)) (* b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-hash-extend #\[
  (lambda (chr port)
    (define (eat-whitespace port)
      (when (char-whitespace? (lookahead-char port))
        (read-char port)
        (eat-whitespace port)))

    (define x (read port))
    (define y (read port))

    (eat-whitespace port)
    (if (eq? (lookahead-char port) #\])
      (begin (read-char port) `(vec2 ,x ,y))
      (let ((z (read port)))
        (eat-whitespace port)
        (if (eq? (lookahead-char port) #\])
          (begin (read-char port) `(vec3 ,x ,y ,z))
          (error "Expected terminating #\\]"))))))
