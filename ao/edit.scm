(define-module (ao edit))

(use-modules (system foreign))
(use-modules (ao jit))
(load-extension "libao" "libao_init")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (show-shape f)
    (gl-window (jit f)))

