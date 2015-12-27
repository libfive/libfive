(define-module (ao edit))

(use-modules (system foreign))
(use-modules (ao jit) (ao lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (show-shape f)
    (gl-window (jit f)))

