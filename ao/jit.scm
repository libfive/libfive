(define-module (ao jit))

(use-modules (system foreign))
(use-modules (ice-9 common-list))
(use-modules (ao lib) (ao ptr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is our local store.  When populated, it's an opaque pointer to a
;; Store object (created in C++ with store_new)
(define store #nil)

;; Converts the given argument to a token
;; Based on argument type, the following behavior occurs
;; - A number is converted to constant
;; - A symbol and further arguments is converted to an operator
(define-public (make-token a . args)
    "Creates a token in the global store.
A token is returned without changes
A number is converted to a constant
A symbol and further arguments are converted to an operation"
    (if (= 0 (length args))
        (cond ((tagged-ptr? 'Token a) a)
              ((number? a) (token_const store a))
              (else (error "Failed to construct token" a)))
        (token_op store a (map make-token args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (interval? i)
    "Check to see if the given argument is a valid Interval"
    (and (pair? i) (not (list? i)) (number? (car i)) (number? (cdr i))))

(define-public (wrap-tree t)
    " Wraps a tagged tree pointer in a callable interface "
    (lambda (x y z) ;; Generic evaluator that dispatches based on argument type
        (cond ((every interval? (list x y z))
                    (tree_eval_interval t x y z))
              ((every number? (list x y z))
                    (tree_eval_double t x y z))
              (else (error "Input arguments are of invalid types")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (jit f)
    "Compile an arithmetic lambda function to a bare tree pointer"
    (set! store (store_new))
    (let* ((x (token_x store))
           (y (token_y store))
           (z (token_z store))
           (root (make-token (f x y z)))
           (out (tree_new store root)))
       (set! store #nil)
       out))
