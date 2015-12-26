(define-module (ao editor))

(use-modules (system foreign))
(use-modules (ao jit))
(load-extension "libao" "libao_init")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (show-tree f)
    (gl-window (tree-ptr (to-tree f))))

