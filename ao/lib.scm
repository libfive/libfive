(define-module (ao lib))

(load-extension "lib/libao" "libao_init")

(export make-store)
(export make-tree)

(export tree-eval-double)
(export tree-eval-interval)

(export token-x)
(export token-y)
(export token-z)
(export token-const)
(export token-op)

(export gl-window)
