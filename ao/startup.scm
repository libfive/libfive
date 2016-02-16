(add-to-load-path (string-append (dirname (current-filename)) "/.."))
(use-modules (ao overload) (ao shapes) (ao core) (ao export))
(system "clear")

(define (ao-shapes)
    (let ((docs
        (module-map (lambda (sym var)
            (let ((v (variable-ref var)))
            (if (procedure? v) (procedure-documentation v))))
            (resolve-interface '(ao shapes)))))
    (map (lambda (d) (display d) (newline))
         (sort (filter string? docs) string<?)))
    #t)
