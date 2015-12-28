(define-module (ao edit))

(use-modules (system foreign))
(use-modules (ao jit) (ao lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (ao-show f)
    (show-tree (jit f)))

