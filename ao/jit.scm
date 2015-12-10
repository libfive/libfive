(define-module (ao jit))

(use-modules (system foreign))
(load-extension "libao" "libao_init")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is our local store.  When populated, it's an opaque pointer to a
;; Store object (created in C++ with make-store)
(define store #nil)

;; Converts the given argument to a token
;; Based on argument type, the following behavior occurs
;; - A number is converted to constant
;; - A symbol and further arguments is converted an operator
(define-public (make-token a . args)
  (cond ((= 0 (length args))
            (cond ((pointer? a) a)
                  ((number? a) (token-const store a))
                  (else (error "Failed to construct token" a))))
        (else (token-op store a (map make-token args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (jit f)
    (set! store (make-store))
    (let* ((args (car (procedure-minimum-arity f)))
           (x (token-x store))
           (y (token-y store))
           (z (token-z store))
           (root (cond ((= 2 args) (make-token (f x y)))
                       ((= 3 args) (make-token (f x y z)))
                       (else (error "Invalid function arity" f))))
           (out (make-tree store root)))
       (set! store #nil)
       (let ((eval-double (lambda (x y z)
                            (tree-mode-double out 1)
                            (tree-eval-double out x y z)))
             (eval-interval (lambda (x y z)
                            (tree-mode-interval out 1)
                            (tree-eval-interval out x y z))))
             (interval? (lambda (i) (and (pair? i) (not (list? i)))))
           (cond ((= 2 args) (lambda (x y)
                            (cond ((interval? x) (eval-interval x y '(0 . 0)))
                                   (else         (eval-double x y 0)))))
                 ((= 3 args) (lambda (x y z)
                            (cond ((interval? x) (eval-interval x y z))
                                   (else         (eval-double x y z)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
