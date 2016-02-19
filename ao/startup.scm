(add-to-load-path (string-append (dirname (current-filename)) "/.."))

(use-modules (ice-9 regex))
(use-modules (system repl common))
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

(display     "         .8.
        .888.
       :88888.          ,o8888o.
      . `88888.      . 8888   `88.
     .8. `88888.    ,8 8888     `8b
    .8`8. `88888.   88 8888      `8b
   .8' `8. `88888.  88 8888      ,8P
  .8'   `8. `88888. `8 8888     ,8P
 .888888888. `88888. ` 8888   ,88'
.8'       `8. `88888.   `88888P'
       (c) 2015 Matt Keeter

REPL is provided by ")

(set! *version* (regexp-substitute #f (string-match "This program" *version*)
                                      'pre "Guile" 'post))

(use-modules (ice-9 readline))
(activate-readline)
