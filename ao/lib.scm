(define-module (ao lib))

(define ao-load-path (getenv "AO_LOAD_PATH"))
(define ao-lib-name "libao")

(load-extension (if ao-load-path (string-append ao-load-path ao-lib-name)
                                 ao-lib-name)
                "libao_init")

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
