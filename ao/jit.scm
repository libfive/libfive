(define-module (ao jit))

(use-modules (system foreign))
(load-extension "libfab" "init_fab")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is our local store.  When populated, it's an opaque pointer to a
;; Store object (created in C++).
(define store #nil)

;; Converts the given argument to a token
;; Based on argument type, the following behavior occurs
;; - A number is converted to constant
;; - A symbol is converted to variable
;; - A symbol and further arguments is converted to operator
(define-public (make-token a . args)
  (cond ((= 0 (length args))
            (cond ((pointer? a) a)
                  ((number? a) (make-num-token store a))
                  ((symbol? a) (make-var-token store a))))
  (else (make-op-token store a (map make-token args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (jit f)
  (let ((args (car (procedure-minimum-arity f))))
    (set! store (make-store))
    (cond ((= 2 args) (make-token (f 'x 'y)))
          ((= 3 args) (make-token (f 'x 'y 'z)))
          (else (error "Invalid function arity" f)))
    store))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
