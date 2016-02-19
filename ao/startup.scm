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

(display     "         \x1b[1m.8.
        .888.
       :88888.          ,o8888o.
      . `88888.      . 8888   `88.
     .8. `88888.    ,8 8888     `8b
    .8`8. `88888.   88 8888      `8b
   .8' `8. `88888.  88 8888      ,8P
  .8'   `8. `88888. `8 8888     ,8P
 .888888888. `88888. ` 8888   ,88'
.8'       `8. `88888.   `88888P'
       \x1b[0m(c) 2015 Matt Keeter

REPL is provided by ")

(set! *version* (regexp-substitute #f (string-match "This program" *version*)
                                      'pre "Guile" 'post))

(use-modules (ice-9 readline))
(activate-readline)

;; Patch the repl-welcome function so that it also modifies the prompt
(define (ao-prompt repl)
    (format #f "\x1b[34mAo~A\x1b[36m>\x1b[0m "
            (let ((level (length (cond
                                  ((fluid-ref *repl-stack*) => cdr)
                                  (else '())))))
              (if (zero? level) "" (format #f " \x1b[31m[~a]" level)))))

(define repl-welcome- repl-welcome)
(set! repl-welcome
    (lambda (repl)
        (repl-option-set! repl 'prompt ao-prompt)
        (repl-default-option-set! 'prompt ao-prompt)
        (repl-welcome- repl)))
