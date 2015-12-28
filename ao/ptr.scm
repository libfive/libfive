(define-module (ao ptr))

(use-modules (system foreign))

(define-public (tagged-ptr? type p)
  (and (pair? p) (eq? type (car p)) (pointer? (cdr p))))
